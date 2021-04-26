#lang racket/base

(require racket/contract
         racket/generic
         rcon/private/packet
         rcon/private/util)

(provide
  gen:to-rcon-packet
  get-rcon-packet-type
  get-rcon-packet-payload
  to-rcon-packet?
  (contract-out
    [->rcon-packet (-> to-rcon-packet? u32/c rcon-packet?)]))

(define-generics to-rcon-packet
  [get-rcon-packet-type to-rcon-packet]
  [get-rcon-packet-payload to-rcon-packet]
  #:fallbacks
  [(define (get-rcon-packet-type p) 2)])

(define (->rcon-packet pck pck-id)
  (rcon-packet pck-id
               (get-rcon-packet-type pck)
               (get-rcon-packet-payload pck)))

