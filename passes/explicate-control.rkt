#lang racket
(require "../utilities.rkt")
(require "../type-check-Cvec.rkt")
(provide explicate-control)

;; explicate-control : R1 -> C0
(define (explicate-control p)
  (match p
    [(Program info e)
     (define-values (start-block blocks) (explicate-tail e '()))
     (type-check-Cvec (CProgram info (cons (cons 'start start-block) blocks)))]))

; e is in tail position
(define (explicate-tail e blocks)
  (match e
    [(Let x rhs body)
     (define-values (cont^ blocks^) (explicate-tail body blocks))
     (explicate-assign rhs x cont^ blocks^)]
    [(If e1 e2 e3)
     (define-values (els-cont blocks^) (explicate-tail e3 blocks))
     (define-values (thn-cont blocks^^) (explicate-tail e2 blocks^))
     (explicate-pred e1 thn-cont els-cont blocks^^)]
    [(Begin (list e1) final-e)
     (define-values (cont^ blocks^) (explicate-tail final-e blocks))
     (explicate-effect e1 cont^ blocks^)]
    [(Begin (list e1 es ...) final-e)
     (define-values (cont^ blocks^) (explicate-tail (Begin es final-e) blocks))
     (explicate-effect e1 cont^ blocks^)]
    [_ (values (Return e) blocks)]))

; Use this function to avoid repeating and also avoid trivial blocks
(define (wrap-with-goto block blocks)
  (cond
    [(Goto? block) (values block blocks)]
    [else (define label (gensym "block"))
          (values (Goto label) (cons `(,label . ,block) blocks))]))

; e is in assignment position
(define (explicate-assign e x cont blocks)
  (match e
    #;[(HasType e1 type) (explicate-assign e1 x cont blocks)]
    [(Let y rhs body)
     (define-values (cont^ blocks^) (explicate-assign body x cont blocks))
     (explicate-assign rhs y cont^ blocks^)]
    [(If e1 e2 e3)
     (define-values (cont^ blocks^) (wrap-with-goto cont blocks))
     (define-values (els-cont blocks^^) (explicate-assign e3 x cont^ blocks^))
     (define-values (thn-cont blocks^^^) (explicate-assign e2 x cont^ blocks^^))
     (explicate-pred e1 thn-cont els-cont blocks^^^)]
    [(SetBang y rhs)
     (define-values (cont^ blocks^) (explicate-assign (Void) x cont blocks))
     (explicate-assign rhs y cont^ blocks^)]
    [(Begin (list e1) final-e)
     (define-values (cont^ blocks^) (explicate-assign final-e x cont blocks))
     (explicate-effect e1 cont^ blocks^)]
    [(Begin (list e1 es ...) final-e)
     (define-values (cont^ blocks^) (explicate-assign (Begin es final-e) x cont blocks))
     (explicate-effect e1 cont^ blocks^)]
    [(WhileLoop cnd body)
     (define-values (cont^ blocks^) (explicate-assign (Void) x cont blocks))
     (define loop-label (gensym 'loop))
     (define-values (body-block blocks^^) (explicate-effect body (Goto loop-label) blocks^))
     (define-values (cnd-block blocks^^^) (explicate-pred cnd body-block cont^ blocks^^))
     (values (Goto loop-label) (cons `(,loop-label . ,cnd-block) blocks^^^))]
    [_ (values (Seq (Assign (Var x) e) cont) blocks)]))

; e is in predicate position
(define (explicate-pred e thn-block els-block blocks)
  (match e
    [(Let x rhs body)
     (define-values (cont^ blocks^) (explicate-pred body thn-block els-block blocks))
     (explicate-assign rhs x cont^ blocks^)]
    [(If e1 e2 e3)
     (define-values (els-block^ blocks^) (wrap-with-goto els-block blocks))
     (define-values (thn-block^ blocks^^) (wrap-with-goto thn-block blocks^))
     (define-values (cont^ blocks^^^) (explicate-pred e2 thn-block^ els-block^ blocks^^))
     (define-values (cont^^ blocks^^^^) (explicate-pred e3 thn-block^ els-block^ blocks^^^))
     (explicate-pred e1 cont^ cont^^ blocks^^^^)]
    [(Bool b) (values (if b thn-block els-block) blocks)]
    [(Var x) (explicate-pred (Prim 'eq? (list e (Bool #t))) thn-block els-block blocks)]
    [(Prim 'not (list e))
     (explicate-pred (Prim 'eq? (list e (Bool #f))) thn-block els-block blocks)]
    [(Begin (list e1) final-e)
     (define-values (cont^ blocks^) (explicate-pred final-e thn-block els-block blocks))
     (explicate-effect e1 cont^ blocks^)]
    [(Begin (list e1 es ...) final-e)
     (define-values (cont^ blocks^)
       (explicate-pred (Begin es final-e) thn-block els-block blocks))
     (explicate-effect e1 cont^ blocks^)]
    [_
     (define-values (els-block^ blocks^) (wrap-with-goto els-block blocks))
     (define-values (thn-block^ blocks^^) (wrap-with-goto thn-block blocks^))
     (values (IfStmt e thn-block^ els-block^) blocks^^)]))

; e is in side-effect position
(define (explicate-effect e cont blocks)
  (match e
    [(or (Int _) (Var _) (Bool _) (Void) (GlobalValue _))
     (values cont blocks)]
    [(or (Collect _) (Allocate _ _)
         (Prim 'read _) (Prim 'vector-set! _))
     (values (Seq e cont) blocks)]
    [(Prim _ _) (values cont blocks)]
    [(Let x rhs body)
     (define-values (cont^ blocks^) (explicate-effect body cont blocks))
     (explicate-assign rhs x cont^ blocks^)]
    [(If e1 e2 e3)
     (define-values (els-block blocks^) (explicate-effect e3 cont blocks))
     (define-values (thn-block blocks^^) (explicate-effect e2 cont blocks^))
     (explicate-pred e1 thn-block els-block blocks^^)]
    [(SetBang x rhs)
     (explicate-assign rhs x cont blocks)]
    [(Begin (list e1) final-e)
     (define-values (cont^ blocks^) (explicate-effect final-e cont blocks))
     (explicate-effect e1 cont^ blocks^)]
    [(Begin (list e1 es ...) final-e)
     (define-values (cont^ blocks^) (explicate-effect (Begin es final-e) cont blocks))
     (explicate-effect e1 cont^ blocks^)]
    [(WhileLoop cnd body)
     (define loop-label (gensym 'loop))
     (define-values (body-block blocks^) (explicate-effect body (Goto loop-label) blocks))
     (define-values (cnd-block blocks^^) (explicate-pred cnd body-block cont blocks^))
     (values (Goto loop-label) (cons `(,loop-label . ,cnd-block) blocks^^))]))
