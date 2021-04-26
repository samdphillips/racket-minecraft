#lang racket/base

(require racket/contract
         rcon/private/util)

(provide
 (contract-out
  [struct rcon-packet
    ([id u32/c]
     [type u32/c]
     [payload bytes?])]
  [rcon-send (-> rcon-connection? rcon-packet? any)]
  [rcon-recv (-> rcon-connection? rcon-packet?)]))

(require rcon/private/connection
         rcon/private/lowio)

(struct rcon-packet (id type payload) #:transparent)

(define (rcon-send conn pck)
  (write-packet (rcon-packet-id pck)
                (rcon-packet-type pck)
                (rcon-packet-payload pck)
                (rcon-connection-out conn)))

(define (rcon-recv conn)
  (call-with-values
   (lambda ()
     (read-packet (rcon-connection-in conn)))
   rcon-packet))

