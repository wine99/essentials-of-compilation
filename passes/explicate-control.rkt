#lang racket
(require "../utilities.rkt")
(require "../type-check-Cvar.rkt")
(provide explicate-control)

;; explicate-control : R1 -> C0
(define (explicate-control p)
  (match p
    [(Program info e)
     (type-check-Cvar (CProgram info `((start . ,(explicate-tail e)))))]))

(define (explicate-tail e)
  (match e
    [(Let x rhs body)
     (explicate-assign rhs x (explicate-tail body))]
    [_ (Return e)]))

(define (explicate-assign e x cont)
  (match e
    [(Let y rhs body)
     (explicate-assign rhs y (explicate-assign body x cont))]
    [_ (Seq (Assign (Var x) e) cont)]))
