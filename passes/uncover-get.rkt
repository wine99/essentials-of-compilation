#lang racket
(require "../utilities.rkt")
(provide uncover-get!)

(define (uncover-get! p)
  (match p
    [(Program info e)
     (define set!-vars (collect-set! e))
     (Program info
              ((uncover-get!-exp set!-vars) e))]))

(define (collect-set! e)
  (match e
    [(or (Int _) (Var _) (Bool _) (Void)) (set)]
    [(Prim 'read _) (set)]
    [(Prim op es)
     (apply set-union (for/list ([e es]) (collect-set! e)))]
    [(Let x rhs body)
     (set-union (collect-set! rhs) (collect-set! body))]
    [(If e1 e2 e3)
     (set-union (collect-set! e1) (collect-set! e2) (collect-set! e3))]
    [(SetBang x e)
     (set-union (set x) (collect-set! e))]
    [(Begin es final-e)
     (apply set-union
            (for/list ([e (append es (list final-e))]) (collect-set! e)))]
    [(WhileLoop cnd body)
     (set-union (collect-set! cnd) (collect-set! body))]))

(define ((uncover-get!-exp set!-vars) e)
  (define recur (uncover-get!-exp set!-vars))
  (match e
    [(Var x)
     (if (set-member? set!-vars x)
         (GetBang x)
         (Var x))]
    [(or (Int _) (Bool _) (Void)) e]
    [(Prim op es) (Prim op (for/list ([e es]) (recur e)))]
    [(Let x rhs body) (Let x (recur rhs) (recur body))]
    [(If e1 e2 e3) (If (recur e1) (recur e2) (recur e3))]
    [(SetBang x e) (SetBang x (recur e))]
    [(Begin es final-e) (Begin (for/list ([e es]) (recur e)) (recur final-e))]
    [(WhileLoop cnd body) (WhileLoop (recur cnd) (recur body))]))
