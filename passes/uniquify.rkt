#lang racket
(require "../utilities.rkt")
(provide uniquify)

;; uniquify : R1 -> R1
(define (uniquify p)
  (match p
    [(ProgramDefs info defs)
     (define top-level
       (for/hash ([def defs])
         (define name (Def-name def))
         (if (eq? name 'main)
             (values 'main 'main)
             (values (Def-name def) (gensym (Def-name def))))))
     (ProgramDefs
      info
      (for/list ([def defs])
        (match def
          [(Def name param* rty info body)
           (define env
             (for/fold ([env top-level])
                       ([(x t) (in-dict param*)])
               (dict-set env x (gensym x))))
           (Def (dict-ref top-level name)
                (for/list ([(x t) (in-dict param*)])
                  (cons (dict-ref env x) t))
                rty
                info
                ((uniquify-exp env) body))])))]))


(define ((uniquify-exp env) e)
  (define recur (uniquify-exp env))
  (match e
    [(or (Int _) (Bool _) (Void)) e]
    [(Var x) (Var (dict-ref env x))]
    [(Let x e body)
     (let ([new-x (gensym x)])
       (Let new-x
            (recur e)
            ((uniquify-exp (dict-set env x new-x)) body)))]
    [(Prim op es)
     (Prim op (map recur es))]
    [(If e1 e2 e3)
     (If (recur e1) (recur e2) (recur e3))]
    [(SetBang x e)
     (SetBang (dict-ref env x) (recur e))]
    [(Begin es final-e)
     (Begin (map recur es) (recur final-e))]
    [(WhileLoop cnd body) (WhileLoop (recur cnd) (recur body))]
    [(HasType e type) (HasType (recur e) type)]
    [(Apply fun args)
     (Apply (recur fun) (map recur args))]
    [(Lambda param* rty body)
     (define-values (new-param* new-dict-items)
       (for/lists (l1 l2) ([x:t param*])
         (match x:t
           [`(,x : ,t)
            (define new-x (gensym x))
            (values `(,new-x : ,t)
                    (cons x new-x))])))
     (Lambda
      new-param*
      rty
      ((uniquify-exp (dict-set-all env new-dict-items)) body))]))
