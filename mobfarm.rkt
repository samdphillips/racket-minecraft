#lang racket/base

(require racket/format
         racket/pretty
         rcon/private/connection
         rcon/private/packet
         rcon/private/packet-conv
         rcon/private/rpc)

(define rcon (make-tcp-rcon-connection "localhost" 25575))

(struct block (name state) #:transparent)

(define (block->string b)
  (define s (block-state b))
  (~a (block-name b)
      (if (hash-empty? s)
          ""
          (~a "["
              (apply ~a #:separator ","
                     (for/list ([(k v) (in-hash s)])
                       (~a k "=" v)))
              "]"))))

(struct fill
  (x0 y0 z0 x1 y1 z1 block)
  #:transparent
  #:methods gen:to-rcon-packet
  [(define (get-rcon-packet-payload cmd)
     (string->bytes/latin-1
      (~a #:separator " "
          "fill"
          (fill-x0 cmd)
          (fill-y0 cmd)
          (fill-z0 cmd)
          (fill-x1 cmd)
          (fill-y1 cmd)
          (fill-z1 cmd)
          (block->string (fill-block cmd)))))])

(struct set-block
  (x y z block)
  #:transparent
  #:methods gen:to-rcon-packet
  [(define (get-rcon-packet-payload cmd)
     (string->bytes/latin-1
      (~a #:separator " "
          "setblock"
          (set-block-x cmd)
          (set-block-y cmd)
          (set-block-z cmd)
          (block->string (set-block-block cmd)))))])

(define-syntax-rule (define-block name)
  (define (name) (block (~a 'name) (hash))))

(define-block air)
(define-block water)
(define-block stone)

(define (trapdoor #:open open #:facing facing #:half half #:kind [kind "oak"])
  (block (format "~a_trapdoor" kind)
         (hash "half" half "open" open "facing" facing)))

(define (clear-area bx by bz)
  (list
   ;; clear area
   (fill bx by bz (+ bx 20) (+ by 40) (+ bz 20) (air))))

(define (make-room bx by bz)
  (list
   ; main-block
   (fill bx by bz (+ bx 19) (+ by 7) (+ bz 19) (stone))

   ;; channel x axis
   (fill (+ 1 bx) (+ 1 by) (+ 1 8 bz)
         (+ 1 17 bx) (+ 1 5 by) (+ 1 8 1 bz)
         (air))

   ;; channel z axis
   (fill (+ 1 8 bx) (+ 1 by) (+ 1 bz)
         (+ 1 8 1 bx) (+ 1 5 by) (+ 1 17 bz)
         (air))

   ;; spawn area
   (fill (+ 1 bx) (+ 3 by) (+ 1 bz)
         (+ 18 bx) (+ 3 3 by) (+ 18 bz)
         (air))

   ;; water blocks - xW
   (fill (+ 1 bx) (+ 1 by) (+ 1 8 bz)
         (+ 1 bx) (+ 1 by) (+ 1 8 1 bz)
         (water))

   ;; water blocks - xE
   (fill (+ 1 17 bx) (+ 1 by) (+ 1 8 bz)
         (+ 1 17 bx) (+ 1 by) (+ 1 8 1 bz)
         (water))

   ;; water blocks - zN
   (fill (+ 1 8 bx) (+ 1 by) (+ 1 bz)
         (+ 1 8 1 bx) (+ 1 by) (+ 1 bz)
         (water))

   ;; water blocks - zS
   (fill (+ 1 8 bx) (+ 1 by) (+ 1 17 bz)
         (+ 1 8 1 bx) (+ 1 by) (+ 1 17 bz)
         (water))

   ;; remove lid
   ;(fill (+ 1 bx) (+ 7 by) (+ 1 bz) (+ bx 18) (+ by 7) (+ bz 18) (air))

   ;; trapdoors NW/S
   (fill (+ 1 bx) (+ 2 by) (+ 1 8 bz)
         (+ 1 7 bx) (+ 2 by) (+ 1 8 bz)
         (trapdoor #:half "top" #:open 'true #:facing 'south))

   ;; trapdoors NE/S
   (fill (+ 11 bx) (+ 2 by) (+ 1 8 bz)
         (+ 11 7 bx) (+ 2 by) (+ 1 8 bz)
         (trapdoor #:half "top" #:open 'true #:facing 'south))

   ;; trapdoors SW/N
   (fill (+ 1 bx) (+ 2 by) (+ 1 9 bz)
         (+ 1 7 bx) (+ 2 by) (+ 1 9 bz)
         (trapdoor #:half "top" #:open 'true #:facing 'north))

   ;; trapdoors SE/N
   (fill (+ 11 bx) (+ 2 by) (+ 1 9 bz)
         (+ 11 7 bx) (+ 2 by) (+ 1 9 bz)
         (trapdoor #:half "top" #:open 'true #:facing 'north))

   ;; trapdoors NW/E
   (fill (+ 1 8 bx) (+ 2 by) (+ 1 bz)
         (+ 1 8 bx) (+ 2 by) (+ 1 7 bz)
         (trapdoor #:half 'top #:open 'true #:facing 'east))

   ;; trapdoors SW/E
   (fill (+ 1 8 bx) (+ 2 by) (+ 11 bz)
         (+ 1 8 bx) (+ 2 by) (+ 11 7 bz)
         (trapdoor #:half 'top #:open 'true #:facing 'east))

   ;; trapdoors NE/W
   (fill (+ 1 9 bx) (+ 2 by) (+ 1 bz)
         (+ 1 9 bx) (+ 2 by) (+ 1 7 bz)
         (trapdoor #:half 'top #:open 'true #:facing 'west))

   ;; trapdoors SE/W
   (fill (+ 1 9 bx) (+ 2 by) (+ 11 bz)
         (+ 1 9 bx) (+ 2 by) (+ 11 7 bz)
         (trapdoor #:half 'top #:open 'true #:facing 'west))))

(define (make-shaft bx by bz)
  (list (fill bx by bz (+ 3 bx) (+ 23 by) (+ 3 bz) (stone))
        (fill (+ 1 bx) by (+ 1 bz)
              (+ 2 bx) (+ 23 by) (+ 2 bz)
              (air))))

(define (make-farm bx by bz)
  (append
    (make-room (- bx 8) (+ 23 by) (- bz 8))
    (make-shaft bx by bz)))

(define-values (bx by bz) (values -427 64 3650))

(rcon-rpc (lambda ()
            (rcon-packet 0 3 #"minecraft"))
          values)

(for ([c (in-list (make-farm bx by bz))])
  (rcon-rpc rcon (lambda () c) pretty-display))

