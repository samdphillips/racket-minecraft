#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [rcon-connection? (-> any/c boolean?)]
  [rcon-connection-close! (-> rcon-connection? any)]
  [rcon-connection-in (-> rcon-connection? input-port?)]
  [rcon-connection-out (-> rcon-connection? output-port?)]
  [make-tcp-rcon-connection (-> string? port-number? rcon-connection?)]))

(require racket/tcp
         rcon/private/logging)

(struct rcon-connection (in out))

(define (make-tcp-rcon-connection host port)
  (call-with-values
   (lambda ()
     (log-rcon-info "making tcp connection to ~a:~a" host port)
     (tcp-connect host port))
   (lambda (i o)
     (log-rcon-info "connection established")
     (rcon-connection i o))))

(define (rcon-connection-close! conn)
  (log-rcon-info "closing connection")
  (close-input-port (rcon-connection-in conn))
  (close-output-port (rcon-connection-out conn)))

