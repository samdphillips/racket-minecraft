#lang racket/base

(require rcon/private/connection
         rcon/private/packet
         rcon/private/rpc)

(define rcon (make-tcp-rcon-connection "localhost" 25575))

(rcon-rpc rcon
          (lambda ()
            (rcon-packet 0 3 #"minecraft"))
          values)

(rcon-rpc rcon
          (lambda ()
            (rcon-packet 1 2 #"list"))
          values)

