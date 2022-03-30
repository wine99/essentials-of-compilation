#lang racket
(require "../utilities.rkt")
(provide expose-allocation)

(define (expose-allocation p)
  (match p
    [(ProgramDefs info defs)
     (ProgramDefs
      info
      (for/list ([def defs])
        (match def
          [(Def name param* rty info body)
           (Def name param* rty info (expose-exp body))])))]))

; After expose-allocation, the wrapping HasType is not needed anymore.

(define (expose-exp e)
  (match e
    [(HasType (Prim 'vector es) type)
     (define v (gensym 'vector))
     (define len (length es))
     (define vars (for/list ([_ (in-range len)]) (gensym 'tmp)))
     (define exposed-es (map expose-exp es))
     (define alloc-and-init
       (make-alloc-and-init v len type vars exposed-es))
     (expose vars exposed-es alloc-and-init)]
    [(Let x e body)
     (Let x (expose-exp e) (expose-exp body))]
    [(If e1 e2 e3)
     (If (expose-exp e1) (expose-exp e2) (expose-exp e3))]
    [(Prim op es)
     (Prim op (map expose-exp es))]
    [(SetBang x rhs) (SetBang x (expose-exp rhs))]
    [(Begin es final-e)
     (Begin (map expose-exp es) (expose-exp final-e))]
    [(WhileLoop cnd body)
     (WhileLoop (expose-exp cnd) (expose-exp body))]
    [(Apply fun args)
     (Apply (expose-exp fun) (map expose-exp args))]
    [(or (Int _) (Var _) (Bool _) (Void) (GetBang _) (FunRef _ _)) e]))

(define (expose vars exposed-es alloc-and-init)
  (match exposed-es
    ['() alloc-and-init]
    [`(,e . ,rest-es)
     #:when (atm? e)
     (expose (cdr vars) rest-es alloc-and-init)]
    [`(,e . ,rest-es)
     (Let (car vars) e
          (expose (cdr vars) rest-es alloc-and-init))]))

(define (make-alloc-and-init vec-sym len vec-type vars exposed-es)
  (Begin
    (list (If (Prim '< (list (Prim '+ (list (GlobalValue 'free_ptr)
                                            (Int (* 8 (add1 len)))))
                             (GlobalValue 'fromspace_end)))
              (Void)
              (Collect (* 8 (add1 len)))))
    (Let vec-sym (Allocate len vec-type)
         (if (= len 0)
             (Var vec-sym)
             (Begin (for/list ([i (in-naturals)] [e exposed-es] [v vars])
                      (Prim 'vector-set! (list (Var vec-sym)
                                               (Int i)
                                               (if (atm? e)
                                                   e
                                                   (Var v)))))
                    (Var vec-sym))))))
