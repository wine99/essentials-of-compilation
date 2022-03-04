#lang racket
(require "../utilities.rkt")
(require "../type-check-Cif.rkt")
(provide explicate-control)

;; explicate-control : R1 -> C0
(define (explicate-control p)
  (match p
    [(Program info e)
     (define-values (start-block blocks) (explicate-tail e '()))
     (type-check-Cif (CProgram info (cons (cons 'start start-block) blocks)))]))

(define (explicate-tail e blocks)
  (match e
    [(Let x rhs body)
     (define-values (cont^ blocks^) (explicate-tail body blocks))
     (explicate-assign rhs x cont^ blocks^)]
    [(If e1 e2 e3)
     (define-values (cont^ blocks^) (explicate-tail e2 blocks))
     (define-values (cont^^ blocks^^) (explicate-tail e3 blocks^))
     (define thn-label (gensym "block"))
     (define els-label (gensym "block"))
     (explicate-pred e1 (Goto thn-label) (Goto els-label)
                     (append `((,thn-label . ,cont^) (,els-label . ,cont^^))
                             blocks^^))]
    [_ (values (Return e) blocks)]))

(define (explicate-assign e x cont blocks)
  (match e
    [(Let y rhs body)
     (define-values (cont^ blocks^) (explicate-assign body x cont blocks))
     (explicate-assign rhs y cont^ blocks^)]
    [(If e1 e2 e3)
     ; Avoid repeating cont
     (define cont-label (gensym "block"))
     (define blocks^ (cons `(,cont-label . ,cont) blocks))
     (define-values (thn-cont blocks^^) (explicate-assign e2 x (Goto cont-label) blocks^))
     (define-values (els-cont blocks^^^) (explicate-assign e3 x (Goto cont-label) blocks^^))
     (define thn-label (gensym "block"))
     (define els-label (gensym "block"))
     (define blocks^^^^
       (append `((,thn-label . ,thn-cont) (,els-label . ,els-cont)) blocks^^^))
     (explicate-pred e1 (Goto thn-label) (Goto els-label) blocks^^^^)]
    [_ (values (Seq (Assign (Var x) e) cont) blocks)]))

; The 'thn-block' and 'els-block' coming in is always a single goto
(define (explicate-pred e thn-block els-block blocks)
  (match e
    [(Let x rhs body)
     (define-values (cont^ blocks^) (explicate-pred body thn-block els-block blocks))
     (explicate-assign rhs x cont^ blocks^)]
    [(If e1 e2 e3)
     (define-values (cont^ blocks^) (explicate-pred e2 thn-block els-block blocks))
     (define-values (cont^^ blocks^^) (explicate-pred e3 thn-block els-block blocks^))
     (define thn-label (gensym "block"))
     (define els-label (gensym "block"))
     (explicate-pred e1 (Goto thn-label) (Goto els-label)
                     (append `((,thn-label . ,cont^) (,els-label . ,cont^^))
                             blocks^^))]
    [(Bool b) (values (if b thn-block els-block) blocks)]
    [(Var x) (explicate-pred (Prim 'eq? (list e (Bool #t))) thn-block els-block blocks)]
    [(Prim 'not (list e)) (explicate-pred e els-block thn-block blocks)]
    [_ (values (IfStmt e thn-block els-block) blocks)]))
