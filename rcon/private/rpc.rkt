#lang racket/base

(require racket/contract)

(provide
  (contract-out
    [rcon-rpc (-> rcon-connection?
                  (-> (or/c rcon-packet? to-rcon-packet?))
                  (-> (listof rcon-packet?) any)
                  any)]))

(require racket/match
         rcon/private/connection
         rcon/private/packet
         rcon/private/packet-conv)

(define (rcon-rpc conn sendf recvf)
  ;; XXX: maybe rcon-connection should track a sequence number
  (define pck
    (let ([pck (sendf)])
      (cond
        [(rcon-packet? pck) pck]
        [else (->rcon-packet pck 0)])))
  (define pck-id (rcon-packet-id pck))
  (define trailer-id (add1 pck-id))
  (define trailer
    (rcon-packet trailer-id 100 #""))
  (rcon-send conn pck)
  (sleep 0.5)
  (rcon-send conn trailer)
  (define rpcks
    (let recv-packets ()
      (define rpck (rcon-recv conn))
      (match (rcon-packet-id rpck)
        [(== pck-id) (cons rpck (recv-packets))]
        [(== trailer-id) null])))
  (recvf rpcks))

