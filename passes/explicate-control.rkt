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
     (explicate-pred e1 cont^ cont^^ blocks^^)]
    [_ (values (Return e) blocks)]))

; Use this function to avoid repeating and also avoid trivial blocks
(define (wrap-with-goto block blocks)
  (cond
    [(Goto? block) (values block blocks)]
    [else (define label (gensym "block"))
          (values (Goto label) (cons `(,label . ,block) blocks))]))

(define (explicate-assign e x cont blocks)
  (match e
    [(Let y rhs body)
     (define-values (cont^ blocks^) (explicate-assign body x cont blocks))
     (explicate-assign rhs y cont^ blocks^)]
    [(If e1 e2 e3)
     (define-values (cont^ blocks^) (wrap-with-goto cont blocks))
     (define-values (els-cont blocks^^) (explicate-assign e3 x cont^ blocks^))
     (define-values (thn-cont blocks^^^) (explicate-assign e2 x cont^ blocks^^))
     (explicate-pred e1 thn-cont els-cont blocks^^^)]
    [_ (values (Seq (Assign (Var x) e) cont) blocks)]))

; The 'thn-block' and 'els-block' coming in is always a single goto
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
    [(Prim 'not (list e)) (explicate-pred e els-block thn-block blocks)]
    [_
     (define-values (els-block^ blocks^) (wrap-with-goto els-block blocks))
     (define-values (thn-block^ blocks^^) (wrap-with-goto thn-block blocks^))
     (values (IfStmt e thn-block^ els-block^) blocks^^)]))
