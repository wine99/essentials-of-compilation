#lang racket
(require "../utilities.rkt")
(provide shrink)

(define (shrink p)
  (match p
    [(ProgramDefsExp info defs e)
     (ProgramDefs
      info
      (append
       (for/list ([def defs])
         (match def
           [(Def name param* rty info body)
            (Def name param* rty info (shrink-exp body))]))
       (list (Def 'main '() 'Integer '() (shrink-exp e)))))]))

(define (shrink-exp e)
  (match e
    [(or (Int _) (Var _) (Bool _) (Void)) e]
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
    [(Prim op es) (Prim op (map shrink-exp es))]
    [(SetBang x e) (SetBang x (shrink-exp e))]
    [(Begin '() final-e)
     (shrink-exp final-e)]
    [(Begin es final-e)
     (Begin (map shrink-exp es) (shrink-exp final-e))]
    [(WhileLoop cnd body) (WhileLoop (shrink-exp cnd) (shrink-exp body))]
    [(HasType e type) (HasType (shrink-exp e) type)]
    [(Apply fun args) (Apply (shrink-exp fun) (map shrink-exp args))]
    [(Lambda param* rty body) (Lambda param* rty (shrink-exp body))]))
