#lang racket/base

(require racket/contract
         rcon/private/logging
         rcon/private/util)

(provide
 (contract-out
  [write-packet (-> u32/c u32/c bytes? output-port? any)]
  [read-packet (-> input-port? (values u32/c u32/c bytes?))]))

(define (int->bytes n)
  (integer->integer-bytes n 4 #f #f))

(define (bytes->int b i)
  (integer-bytes->integer b #f #f i (+ 4 i)))

(define (write-packet id type payload outp)
  (log-rcon-debug "write-packet ~a ~a ~s" id type payload)
  (define packet
    (bytes-append (int->bytes id)
                  (int->bytes type)
                  payload
                  (bytes 0 0)))
  (write-bytes
   (int->bytes (bytes-length packet)) outp)
  (log-rcon-debug "writing packet >>> ~s" packet)
  (write-bytes packet outp)
  (flush-output outp))

(define (read-packet inp)
  (define size
    (bytes->int (read-bytes 4 inp) 0))
  (log-rcon-debug "read size      <<< ~a" size)
  (define packet (read-bytes size inp))
  (log-rcon-debug "read packet    <<< ~s" packet)
  (values (bytes->int packet 0)
          (bytes->int packet 4)
          (subbytes packet 8 (- size 2))))

(module* toy #f
  (require racket/tcp)
  (define-values (rcon-in rcon-out)
    (tcp-connect "localhost" 25575))

  (write-packet 1234 3 #"minecraft" rcon-out)
  (read-packet rcon-in)

  ; (write-packet 5432 2 #"fill -540 66 3670 -545 66 3672 stone_brick_slab[type=top]" rcon-out)
  ; (write-packet 5432 2 #"fill -540 66 3670 -545 66 3672 air" rcon-out)
  ; (read-packet rcon-in)
  )

