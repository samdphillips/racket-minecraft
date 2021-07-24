#lang racket/base

(provide (all-defined-out))

(define (bytes->int b signed?)
  (integer-bytes->integer b signed? #t))

(define (bytes->real b _signed?)
  (floating-point-bytes->real b #t))

(define ((make-prim-reader size [signed? #f] #:convert [convert bytes->int]) inp)
  (convert (read-bytes size inp) signed?))

(define read-short  (make-prim-reader 2 #t))
(define read-ushort (make-prim-reader 2 #f))
(define read-int    (make-prim-reader 4 #t))
(define read-uint   (make-prim-reader 4 #f))
(define read-long   (make-prim-reader 8 #t))
(define read-float  (make-prim-reader 4 #:convert bytes->real))
(define read-double (make-prim-reader 8 #:convert bytes->real))

(define (read-end inp) 0)

(define (read-byte-array inp)
  (define amount (read-uint inp))
  (read-bytes amount inp))

(define ((make-int-array-reader unit) inp)
  (define amount (read-uint inp))
  (define size (* unit amount))
  (define bytes (read-bytes size inp))
  (for/vector #:length amount ([i (in-range 0 size unit)]
                               [j (in-range unit size unit)])
    (integer-bytes->integer bytes #t #t i j)))

(define read-int-array  (make-int-array-reader 4))
(define read-long-array (make-int-array-reader 8))

(define (read-string inp)
  (define amount (read-ushort inp))
  (define bytes (read-bytes amount inp))
  (bytes->string/utf-8 bytes))

(define (read-list inp)
  (define tag (read-byte inp))
  (define read1 (vector-ref tag-dispatch tag))
  (define amount (read-uint inp))
  (for/list ([_i (in-range amount)])
    (read1 inp)))

(define (read-compound inp)
  (define (read1 cmpd)
    (define tag (read-byte inp))
    (cond
      [(zero? tag) cmpd]
      [else
       (define name (read-string inp))
       (define value
         ((vector-ref tag-dispatch tag) inp))
       (read1 (hash-set cmpd name value))]))
  (read1 (hash)))

(define tag-dispatch
  (vector read-end
          read-byte
          read-short
          read-int
          read-long
          read-float
          read-double
          read-byte-array
          read-string
          read-list
          read-compound
          read-int-array
          read-long-array))

(define (read-nbt inp)
  (define tag (read-byte inp))
  (define name (read-string inp))
  (define value (read-compound inp))
  (cons name value))

(module* main #f
  (require racket/file
           racket/port
           racket/pretty
           nbt/decompress)
  
  (define f (vector-ref (current-command-line-arguments) 0))
  (define b (file->bytes f))
  (call-with-input-bytes b
                         (lambda (inp)
                           (pretty-print (read-nbt (gunzip-port inp)))))
  
  #;
  (call-with-input-file (vector-ref (current-command-line-arguments) 0)
    (lambda (inp)
      (define dec-inp (inflate-port inp))
      (pretty-print (read-nbt dec-inp)))))

