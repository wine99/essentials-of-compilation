#lang racket
(require "../utilities.rkt")
(provide shrink)

(define (shrink p)
  (match p
    [(Program info e) (Program info (shrink-exp e))]))

(define (shrink-exp e)
  (match e
    [(Prim 'and (list e1 e2))
     (If (shrink-exp e1)
         (shrink-exp e2)
         (Bool #f))]
    [(Prim 'or (list e1 e2))
     (If (shrink-exp e1)
         (Bool #t)
         (shrink-exp e2))]
    [_ e]))