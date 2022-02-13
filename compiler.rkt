#lang racket
(require racket/set racket/stream)
(require racket/fixnum)
(require "interp-Lint.rkt")
(require "interp-Lvar.rkt")
(require "interp-Cvar.rkt")
(require "utilities.rkt")
(provide (all-defined-out))


;; Next we have the partial evaluation pass described in the book.
(define (pe-neg r)
  (match r
    [(Int n) (Int (fx- 0 n))]
    [else (Prim '- (list r))]))

(define (pe-add r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int (fx+ n1 n2))]
    [(_ _) (Prim '+ (list r1 r2))]))

(define (pe-exp e)
  (match e
    [(Int n) (Int n)]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (pe-neg (pe-exp e1))]
    [(Prim '+ (list e1 e2)) (pe-add (pe-exp e1) (pe-exp e2))]))

(define (pe-Lint p)
  (match p
    [(Program info e) (Program info (pe-exp e))]))

(define (uniquify-exp env)
  (lambda (e)
    (match e
      [(Var x)
       (let ([p (assoc x env)])
         (if (and p (eq? (car p) x))
             (Var (cdr p))
             ;; TODO maybe check somewhere else
             (error 'syntax-error "unbound identifier " x)))]
      ;; TODO 重新构造 (Int n) 更高效还是直接返回 e ？
      [(Int n) (Int n)]
      [(Let x e body)
       (let ([new-x (gensym)])
         (Let new-x
              ((uniquify-exp env) e)
              ((uniquify-exp (cons (cons x new-x) env)) body)))]
      [(Prim op es)
       (Prim op (for/list ([e es]) ((uniquify-exp env) e)))])))

;; uniquify : R1 -> R1
(define (uniquify p)
  (match p
    [(Program info e) (Program info ((uniquify-exp '()) e))]))


;; remove-complex-opera* : R1 -> R1
(define (remove-complex-opera* p)
  (match p
    [(Program info e) (Program info (rco-exp e))]))

(define (rco-atom e)
  (match e
    [(Var x) (values (Var x) '())]
    [(Int n) (values (Int n) '())]
    [(Let x rhs body)
     (define rcoed-rhs (rco-exp rhs))
     (define-values (rcoed-body pairs) (rco-atom body))
     (values rcoed-body (cons `(,x . ,rcoed-rhs) pairs))]
    [(Prim op es)
     (define-values
       (rcoed-es _pairs)
       (for/lists (l1 l2) ([e es]) (rco-atom e)))
     (define pairs (append* _pairs))
     (define tmp (gensym))
     (values (Var tmp) (append pairs `((,tmp . ,(Prim op rcoed-es)))))]))

(define (rco-exp e)
  (match e
    [(Var x) (Var x)]
    [(Int n) (Int n)]
    [(Let x rhs body)
     (Let x (rco-exp rhs) (rco-exp body))]
    [(Prim op es)
     (define-values
       (rcoed-es _pairs)
       (for/lists (l1 l2) ([e es]) (rco-atom e)))
     (define pairs (append* _pairs))
     (make-lets pairs (Prim op rcoed-es))]))

;; make-lets is defined in utilities
;; TODO read code of make-lets in utilities
;(define (make-lets pairs final-exp)
;  (match pairs
;    ['() final-exp]
;    [`((,symbol . ,exp) rest)
;     (Let symbol exp (make-lets rest final-exp))]))


;; explicate-control : R1 -> C0
(define (explicate-control p)
  (match p
    [(Program info e) (CProgram info `((start . ,(explicate-tail e))))]))

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


;; select-instructions : C0 -> pseudo-x86
(define (select-instructions p)
  (error "TODO: code goes here (select-instructions)"))

;; assign-homes : pseudo-x86 -> pseudo-x86
(define (assign-homes p)
  (error "TODO: code goes here (assign-homes)"))

;; patch-instructions : psuedo-x86 -> x86
(define (patch-instructions p)
  (error "TODO: code goes here (patch-instructions)"))

;; prelude-and-conclusion : x86 -> x86
(define (prelude-and-conclusion p)
  (error "TODO: code goes here (prelude-and-conclusion)"))

;; Define the compiler passes to be used by interp-tests and the grader
;; Note that your compiler file (the file that defines the passes)
;; must be named "compiler.rkt"
(define compiler-passes
  `(("uniquify" ,uniquify ,interp-Lvar)
    ("remove complex opera*" ,remove-complex-opera* ,interp-Lvar)
    ("explicate control" ,explicate-control ,interp-Cvar)
    ;; ("instruction selection" ,select-instructions ,interp-x86-0)
    ;; ("assign homes" ,assign-homes ,interp-x86-0)
    ;; ("patch instructions" ,patch-instructions ,interp-x86-0)
    ;; ("prelude-and-conclusion" ,prelude-and-conclusion ,interp-x86-0)
    ))

