#lang racket
(require "../utilities.rkt")
(provide reveal-functions)

(define (reveal-functions p)
  (match p
    [(ProgramDefs info defs)
     (define defs-name-arity
       (for/hash ([def defs])
         (values (Def-name def) (length (Def-param* def)))))
     (set! defs-name-arity (hash-remove defs-name-arity 'main))
     (ProgramDefs
      info
      (for/list ([def defs])
        (match def
          [(Def name param* rty info body)
           (Def name param* rty info
                ((reveal-fun-exp defs-name-arity) body))])))]))

(define ((reveal-fun-exp defs-name-arity) exp)
  (define recur (reveal-fun-exp defs-name-arity))
  (match exp
    [(Var x) (if (hash-ref defs-name-arity x #f)
                 (FunRef x (hash-ref defs-name-arity x))
                 exp)]
    [(Let x e body) (Let x (recur e) (recur body))]
    [(Prim op es) (Prim op (map recur es))]
    [(If e1 e2 e3) (If (recur e1) (recur e2) (recur e3))]
    [(SetBang x e) (SetBang x (recur e))]
    [(Begin es final-e)
     (Begin (map recur es) (recur final-e))]
    [(WhileLoop cnd body) (WhileLoop (recur cnd) (recur body))]
    [(HasType e type) (HasType (recur e) type)]
    [(Apply fun args)
     (Apply (recur fun) (map recur args))]
    [(or (Int _) (Bool _) (Void) (GetBang _)) exp]))
