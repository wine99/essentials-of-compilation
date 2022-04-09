#lang racket
(require "../utilities.rkt")
(require "./uncover-get.rkt")
(provide convert-assignments free-vars)

(define (convert-assignments p)
  (match p
    [(ProgramDefs info defs)
     (ProgramDefs
      info
      (for/list ([def defs])
        (match def
          [(Def name param* rty info body)
           (define F (free-vars body))
           (define A (collect-set! body))
           (define AF (set-intersect A F))
           (define-values (new-param* bs)
             (convert-assign-param param* AF))
           (Def name new-param* rty info
                (make-lets bs ((convert-assign-exp AF) body)))])))]))

(define (free-vars exp)
  (match exp
    [(or (Int _) (Var _) (Bool _) (Void) (GetBang _) (FunRef _ _)) (set)]
    [(Let x e body)
     (set-union (free-vars e) (free-vars body))]
    [(Prim op '()) (set)]
    [(Prim op es) (apply set-union (map free-vars es))]
    [(If e1 e2 e3)
     (set-union (free-vars e1) (free-vars e2) (free-vars e3))]
    [(SetBang x e) (free-vars e)]
    [(Begin es final-e)
     (apply set-union (map free-vars (append es (list final-e))))]
    [(WhileLoop cnd body) (set-union (free-vars cnd) (free-vars body))]
    [(HasType e type) (free-vars e)]
    [(Apply fun args)
     (apply set-union (map free-vars (append (list fun) args)))]
    [(Lambda (list `[,xs : ,ts] ...) rty body)
     (foldl (lambda (x s) (set-remove s x)) (free-vars-lambda body) xs)]))

(define (free-vars-lambda exp)
  (define recur free-vars-lambda)
  (match exp
    [(or (Int _) (Bool _) (Void) (FunRef _ _)) (set)]
    [(or (Var x) (GetBang x)) (set x)]
    [(Let x e body)
     (set-union (recur e)
                (set-remove (recur body) x))]
    [(Prim op '()) (set)]
    [(Prim op es) (apply set-union (map recur es))]
    [(If e1 e2 e3)
     (set-union (recur e1) (recur e2) (recur e3))]
    [(SetBang x e) (set x)]
    [(Begin es final-e)
     (apply set-union (map recur (append es (list final-e))))]
    [(WhileLoop cnd body) (set-union (recur cnd) (recur body))]
    [(HasType e type) (recur e)]
    [(Apply fun args)
     (apply set-union (map recur (append (list fun) args)))]
    [(Lambda (list `[,xs : ,ts] ...) rty body)
     (foldl (lambda (x s) (set-remove s x)) (free-vars-lambda body) xs)]))

(define ((convert-assign-exp AF) exp)
  (define recur (convert-assign-exp AF))
  (match exp
    [(or (Int _) (Bool _) (Void)) exp]
    [(or (Var x) (GetBang x))
     (if (set-member? AF x)
         (Prim 'vector-ref (list (Var x) (Int 0)))
         exp)]
    [(Let x e body)
     (if (set-member? AF x)
         (Let x (Prim 'vector (list (recur e))) (recur body))
         (Let x (recur e) (recur body)))]
    [(SetBang x e)
     (if (set-member? AF x)
         (Prim 'vector-set! (list (Var x) (Int 0) (recur e)))
         (SetBang x (recur e)))]
    [(Prim op es) (Prim op (map recur es))]
    [(If e1 e2 e3) (If (recur e1) (recur e2) (recur e3))]
    [(Begin es final-e) (Begin (map recur es) (recur final-e))]
    [(WhileLoop cnd body) (WhileLoop (recur cnd) (recur body))]
    [(HasType e type) (HasType (recur e) type)]
    [(Apply fun args) (Apply (recur fun) (map recur args))]
    [(Lambda param* rty body)
     (define-values (new-param* bs)
       (convert-assign-param param* AF))
     (Lambda new-param* rty (make-lets bs (recur body)))]))

(define (convert-assign-param param* AF)
  (for/foldr ([new-param* '()]
              [bindings '()])
             ([x:t param*])
    (match x:t
      [`(,x : ,t)
       (cond
         [(set-member? AF x)
          (define new-x (gensym x))
          (values (cons `(,new-x : ,t) new-param*)
                  (cons (cons x (Prim 'vector (list (Var new-x))))
                        bindings))]
         [else (values (cons x:t new-param*) bindings)])])))
