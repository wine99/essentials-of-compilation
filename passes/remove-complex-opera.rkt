#lang racket
(require "../utilities.rkt")
(provide remove-complex-opera*)

;; remove-complex-opera* : R1 -> R1
(define (remove-complex-opera* p)
  (match p
    [(Program info e) (Program info (rco-exp e))]))

; Apply rco-atom to subexpressions that need to be atomic,
; apply rco-exp to these that do not.

; rco-atom returns an atmoic expression and
; a alist mapping temporary variables to complex subexpressions

(define (rco-atom e)
  (define tmp (gensym 'tmp))
  (match e
    [(or (Int _) (Var _) (Bool _) (Void)) (values e '())]
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
    [(or (If _ _ _) (GetBang _) (Begin _ _) (WhileLoop _ _))
     (values (Var tmp) `((,tmp . ,(rco-exp e))))]))

(define (rco-exp e)
  (match e
    [(or (Int _) (Var _) (Bool _) (Void)) e]
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
     (Begin (for/list ([e es]) (rco-exp e)) (rco-exp final-e))]
    [(WhileLoop cnd body)
     (WhileLoop (rco-exp cnd) (rco-exp body))]))

;; make-lets is defined in utilities
;; TODO read code of make-lets in utilities
;(define (make-lets pairs final-exp)
;  (match pairs
;    ['() final-exp]
;    [`((,symbol . ,exp) rest)
;     (Let symbol exp (make-lets rest final-exp))]))
