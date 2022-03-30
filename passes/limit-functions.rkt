#lang racket
(require "../utilities.rkt")
(require "../type-check-Lfun.rkt")
(provide limit-functions)

; This pass need to type-check because new vectors will be created
; and we need type-checker to wrap HasTypes around them.
(define (limit-functions p)
  (match p
    [(ProgramDefs info defs)
     (ProgramDefs info (map limit-def defs))]))

(define N (vector-length arg-registers))
(define M (- N 1))

(define (limit-def def)
  (match def
    [(Def name param* rty info body)
     (define tup (gensym 'tup))
     (define mapping
       (if ((length (Def-param* def)) . > . N)
           (for/hash ([(x t) (in-dict (drop param* M))]
                      [i (in-naturals)])
             (values x i))
           (hash)))
     (define new-param*
       (if ((length (Def-param* def)) . > . N)
           (match param*
             [`([,xs : ,ts] ...)
              (append (take param* M)
                      `([,tup : (Vector ,@(drop ts M))]))])
           param*))
     (Def name new-param* rty info ((limit-exp tup mapping) body))]))

(define ((limit-exp tup mapping) exp)
  (define recur (limit-exp tup mapping))
  (match exp
    [(or (Var x) (GetBang x))
     (if (hash-ref mapping x #f)
         (Prim 'vector-ref (list (Var tup) (Int (hash-ref mapping x #f))))
         exp)]
    [(SetBang x rhs)
     (if (hash-ref mapping x #f)
         (Prim 'vector-set!
               (list (Var tup) (Int (hash-ref mapping x #f)) (recur rhs)))
         (SetBang x (recur rhs)))]
    [(Let x e body) (Let x (recur e) (recur body))]
    [(Prim op es) (Prim op (map recur es))]
    [(If e1 e2 e3) (If (recur e1) (recur e2) (recur e3))]
    [(Begin es final-e)
     (Begin (map recur es) (recur final-e))]
    [(WhileLoop cnd body) (WhileLoop (recur cnd) (recur body))]
    [(HasType e type) (HasType (recur e) type)]
    [(Apply fun args)
     (if ((length args) . > . N)
         (Apply (recur fun)
                (let ([args (map recur args)])
                  (append (take args M)
                          (list (Prim 'vector (drop args M))))))
         (Apply (recur fun) (map recur args)))]
    [(or (Int _) (Bool _) (Void) (FunRef _ _)) exp]))
