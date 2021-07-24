#lang racket/base

(require racket/contract)

(provide
  (contract-out
    [region-chunk (-> region? integer? integer? chunk?)]
    [region-loc->fname (-> path-string? integer? integer? path-string?)]
    [load-region (-> path-string? region?)]
    [chunk-input-port (-> chunk? input-port?)]

    [block-loc->chunk-loc (-> integer? integer? (values integer? integer?))]
    [chunk-loc->region-loc (-> integer? integer? (values integer? integer?))]
    ))

(require racket/file
         racket/format
         racket/match
         racket/math)

(define (qt n d)
  (exact-floor (/ n d)))

(define (block-loc->chunk-loc block-x block-z)
  (values (qt block-x 16) (qt block-z 16)))

(define (chunk-loc->region-loc chunk-x chunk-z)
  (values (qt chunk-x 32) (qt chunk-z 32)))

(struct region (bytes))

(struct chunk (start size region))

(define (region-loc->fname dim-path region-x region-z)
  (build-path dim-path (~a "r." region-x "." region-z ".mca")))

(define (load-region fname)
  (region (file->bytes fname)))

(define (region-chunk-location r chunk-x chunk-z)
  (define norm-chunk-x (modulo chunk-x 32))
  (define norm-chunk-z (modulo chunk-z 32))
  (define entry-offset (* (+ norm-chunk-x (* norm-chunk-z 32)) 4))
  (define entry
    (integer-bytes->integer
     (region-bytes r) #f #t entry-offset (+ 4 entry-offset)))
  (values (* 4096 (bitwise-and #xFFFFFF (arithmetic-shift entry -8)))
          (* 4096 (bitwise-and #xFF entry))))

(define (region-chunk r chunk-x chunk-z)
  (define-values (offset size) (region-chunk-location r chunk-x chunk-z))
  (chunk offset size r))

(define (chunk-nbt-size ch)
  (define b (region-bytes (chunk-region ch)))
  (define o (chunk-start ch))
  (integer-bytes->integer b #f #t o (+ 4 o)))

(define (chunk-nbt-compression ch)
  (define b (region-bytes (chunk-region ch)))
  (define o (chunk-start ch))
  (match (bytes-ref b (+ o 4))
    [2 'zlib]
    [1 'gzip]))

(define (chunk-input-port ch)
  (define b (region-bytes (chunk-region ch)))
  (define start (chunk-start ch))
  (define size (chunk-nbt-size ch))
  (open-input-bytes (subbytes b (+ start 5) (+ start 5 size))))

