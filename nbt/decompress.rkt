#lang racket/base

(provide (all-defined-out))

(require file/gunzip
         racket/format)

;; unexported functions from
;; https://github.com/racket/racket/blob/master/racket/collects/net/git-checkout.rkt

(struct exn:fail:zlib-inflate exn:fail () #:transparent)

(define (raise-zlib-inflate-error name fmt . vals)
  (raise (exn:fail:zlib-inflate (apply format (string-append "~s: " fmt) name vals)
                                (current-continuation-marks))))

(define (read-bytes-exactly what len i)
  (define bstr (read-bytes len i))
  (unless (and (bytes? bstr)
               (= (bytes-length bstr) len))
    (raise-zlib-inflate-error 'zlib-inflate
                              (~a "error getting bytes for ~a\n"
                                  "  expected length: ~a\n"
                                  "  got length: ~a")
                              what
                              len
                              (if (eof-object? bstr)
                                  eof
                                  (bytes-length bstr))))
  bstr)

(define (read-byte-only what i)
  (define c (read-byte i))
  (unless (byte? c)
    (raise-zlib-inflate-error 'zlib-inflate "expected to get a byte for ~a, got end-of-file" what))
  c)

;; ADLER32 implementation
;; https://www.ietf.org/rfc/rfc1950.txt
(define (adler32-through-ports in out)
  (define ADLER 65521)
  (define bstr (make-bytes 4096))
  (let loop ([s1 1] [s2 0])
    (define n (read-bytes! bstr in))
    (cond
      [(eof-object? n)
       (bitwise-ior (arithmetic-shift s2 16) s1)]
      [else
       (write-bytes bstr out 0 n)
       (define-values (new-s1 new-s2)
         (for/fold ([s1 s1]
                    [s2 s2])
                   ([bits (in-bytes bstr 0 n)])
           (define a (modulo (+ s1 bits) ADLER))
           (define b (modulo (+ s2 a) ADLER))
           (values a b)))
       (loop new-s1 new-s2)])))

;; zlib-inflate : input-port output-port
;;  Reads compressed data from `i`, writes uncompressed to `o`
(define (zlib-inflate i o)
  (define cmf (read-byte-only 'zlib-cmf i))
  (define flg (read-byte-only 'zlib-flag i))
  (unless (= 8 (bitwise-and cmf #xF))
    (raise-zlib-inflate-error 'zlib-inflate "compression is not `deflate`"))
  (when (bitwise-bit-set? flg 5)
    ;; read dictid
    (read-bytes-exactly 'dictid 4 i))
  ;; Include adler32 checksum in the pipeline, writing to `o`:
  (define-values (checksum-in checksum-out) (make-pipe 4096))
  (define uncompressed-adler #f)
  (define checksum-thread
    (thread
     (lambda () (set! uncompressed-adler (adler32-through-ports checksum-in o)))))
  ;; Inflate, sending output to checksum (and then to `o`):
  (inflate i checksum-out)
  (close-output-port checksum-out)
  (sync checksum-thread)
  ;; Verify checksum
  (define adler (read-bytes-exactly 'adler-checksum 4 i))
  (unless (= (integer-bytes->integer adler #f #t)
             uncompressed-adler)
    (raise-zlib-inflate-error 'zlib-inflate "adler32 checksum failed"))
  (void))

;; --- end of functions from git-checkout.rkt

(define ((make-coding-reader coder [who #f]) input)
  (define-values (from-pipe to-pipe) (make-pipe))
  (define err #f)
  (define (save-err e) (set! err e))
  (define (do-read buf)
    (when err
      (raise err))
    (define read-size
      (read-bytes! buf from-pipe))
    read-size)
  (define (do-close)
    (close-input-port from-pipe)
    (kill-thread thd)
    (close-input-port input))
  (define thd
    (thread
     (lambda ()
       (with-handlers ([exn:fail? save-err])
         (coder input to-pipe)
         (close-output-port to-pipe)))))
  (make-input-port who do-read #f do-close))

(define gunzip-port (make-coding-reader gunzip-through-ports))
(define zlib-inflate-port (make-coding-reader zlib-inflate))

