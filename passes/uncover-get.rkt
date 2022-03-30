#lang racket
(require "../utilities.rkt")
(provide uncover-get!)

(define (uncover-get! p)
  (match p
    [(ProgramDefs info defs)
     (ProgramDefs
      info
      (for/list ([def defs])
        (match def
          [(Def name param* rty info body)
           (define set!-vars (collect-set! body))
           (Def name param* rty info
                ((uncover-get!-exp set!-vars) body))])))]))

(define (collect-set! e)
  (match e
    [(or (Int _) (Var _) (Bool _) (Void)) (set)]
    [(Prim op '()) (set)]
    [(Prim op es)
     (apply set-union (map collect-set! es))]
    [(Let x rhs body)
     (set-union (collect-set! rhs) (collect-set! body))]
    [(If e1 e2 e3)
     (set-union (collect-set! e1) (collect-set! e2) (collect-set! e3))]
    [(SetBang x e)
     (set-union (set x) (collect-set! e))]
    [(Begin es final-e)
     (apply set-union (map collect-set! (append es (list final-e))))]
    [(WhileLoop cnd body)
     (set-union (collect-set! cnd) (collect-set! body))]
    [(HasType e type) (collect-set! e)]
    [(Apply fun args)
     (apply set-union (map collect-set! (append (list fun) args)))]
    [(or (Collect _) (Allocate _ _) (GlobalValue _)) (set)]))

(define ((uncover-get!-exp set!-vars) e)
  (define recur (uncover-get!-exp set!-vars))
  (match e
    [(Var x)
     (if (set-member? set!-vars x)
         (GetBang x)
         (Var x))]
    [(or (Int _) (Bool _) (Void)) e]
    [(Prim op es) (Prim op (map recur es))]
    [(Let x rhs body) (Let x (recur rhs) (recur body))]
    [(If e1 e2 e3) (If (recur e1) (recur e2) (recur e3))]
    [(SetBang x e) (SetBang x (recur e))]
    [(Begin es final-e) (Begin (map recur es) (recur final-e))]
    [(WhileLoop cnd body) (WhileLoop (recur cnd) (recur body))]
    [(HasType e type) (HasType (recur e) type)]
    [(Apply fun args) (Apply (recur fun) (map recur args))]
    [(or (Collect _) (Allocate _ _) (GlobalValue _)) e]))
