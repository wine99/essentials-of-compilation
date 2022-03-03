#lang racket
(require "../utilities.rkt")
(provide shrink)

(define (shrink p)
  (match p
    [(Program info e) (Program info (shrink-exp e))]))

(define (shrink-exp e)
  (match e
    [(or (Int x) (Var x) (Bool x)) e]
    [(Let x rhs body) (Let x (shrink-exp rhs) (shrink-exp body))]
    [(If e1 e2 e3) (If (shrink-exp e1) (shrink-exp e2) (shrink-exp e3))]
    [(Prim 'and (list e1 e2))
     (If (shrink-exp e1)
         (shrink-exp e2)
         (Bool #f))]
    [(Prim 'or (list e1 e2))
     (If (shrink-exp e1)
         (Bool #t)
         (shrink-exp e2))]
    [(Prim op es) (Prim op (for/list ([e es]) (shrink-exp e)))]))