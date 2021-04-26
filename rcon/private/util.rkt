#lang racket/base

(provide u32/c)

(require racket/contract)

(define u32/c
  (and/c exact-nonnegative-integer?
         (<=/c #xffffffff)))

