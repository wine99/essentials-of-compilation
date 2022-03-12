#lang racket
(require "../utilities.rkt")
(provide uniquify)

;; uniquify : R1 -> R1
(define (uniquify p)
  (match p
    [(Program info e) (Program info ((uniquify-exp '()) e))]))

(define ((uniquify-exp env) e)
  (define recur (uniquify-exp env))
    (match e
      [(Int n) e]
      [(Bool b) e]
      [(Var x) (Var (dict-ref env x))]
      [(Let x e body)
       (let ([new-x (gensym x)])
         (Let new-x
              (recur e)
              ((uniquify-exp (dict-set env x new-x)) body)))]
      [(Prim op es)
       (Prim op (for/list ([e es]) (recur e)))]
      [(If e1 e2 e3)
       (If (recur e1) (recur e2) (recur e3))]))
