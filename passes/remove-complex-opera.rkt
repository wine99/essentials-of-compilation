#lang racket
(require "../utilities.rkt")
(provide remove-complex-opera*)

;; remove-complex-opera* : R1 -> R1
(define (remove-complex-opera* p)
  (match p
    [(ProgramDefs info defs)
     (ProgramDefs
      info
      (for/list ([def defs])
        (match def
          [(Def name param* rty info body)
           (Def name param* rty info (rco-exp body))])))]))

; Apply rco-atom to subexpressions that need to be atomic,
; apply rco-exp to these that do not.

; rco-atom returns an atmoic expression and
; a alist mapping temporary variables to complex subexpressions

(define (rco-atom e)
  (define tmp (gensym 'tmp))
  (match e
    [(or (Int _) (Var _) (Bool _) (Void)) (values e '())]
    [(or (Collect _) (Allocate _ _) (GlobalValue _)
         (AllocateClosure _ _ _))
     (define tmp (gensym 'tmp))
     (values (Var tmp) `((,tmp . ,e)))]
    [(Let x rhs body)
     (define rcoed-rhs (rco-exp rhs))
     (define-values (rcoed-body pairs) (rco-atom body))
     (values rcoed-body (cons `(,x . ,rcoed-rhs) pairs))]
    [(Prim op es)
     (define-values
       (rcoed-es _pairs)
       (for/lists (l1 l2) ([e es]) (rco-atom e)))
     (define pairs (append* _pairs))
     (values (Var tmp) (append pairs `((,tmp . ,(Prim op rcoed-es)))))]
    [(Apply fun args)
     (define-values (rcoed-fun pairs) (rco-atom fun))
     (define-values
       (rcoed-args _pairs)
       (for/lists (l1 l2) ([a args]) (rco-atom a)))
     (values (Var tmp)
             (append pairs
                     (append* _pairs)
                     `((,tmp . ,(Apply rcoed-fun rcoed-args)))))]
    [(or (If _ _ _) (GetBang _) (Begin _ _) (WhileLoop _ _) (FunRef _ _))
     (values (Var tmp) `((,tmp . ,(rco-exp e))))]))

(define (rco-exp e)
  (match e
    [(or (Int _) (Var _) (Bool _) (Void)
         (Collect _) (Allocate _ _) (GlobalValue _)
         (FunRef _ _) (AllocateClosure _ _ _))
     e]
    [(GetBang x) (Var x)]
    [(Let x rhs body)
     (Let x (rco-exp rhs) (rco-exp body))]
    [(If e1 e2 e3)
     (If (rco-exp e1) (rco-exp e2) (rco-exp e3))]
    [(Prim op es)
     (define-values
       (rcoed-es _pairs)
       (for/lists (l1 l2) ([e es]) (rco-atom e)))
     (define pairs (append* _pairs))
     (make-lets pairs (Prim op rcoed-es))]
    [(SetBang x e) (SetBang x (rco-exp e))]
    [(Begin es final-e)
     (Begin (map rco-exp es) (rco-exp final-e))]
    [(WhileLoop cnd body)
     (WhileLoop (rco-exp cnd) (rco-exp body))]
    [(Apply fun args)
     (define-values (rcoed-fun pairs) (rco-atom fun))
     (define-values
       (rcoed-args _pairs)
       (for/lists (l1 l2) ([a args]) (rco-atom a)))
     (make-lets (append pairs (append* _pairs))
                (Apply rcoed-fun rcoed-args))]))
