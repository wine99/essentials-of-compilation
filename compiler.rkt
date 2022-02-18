#lang racket
(require racket/set racket/stream)
(require racket/fixnum)
(require "interp-Lint.rkt")
(require "interp-Lvar.rkt")
(require "interp-Cvar.rkt")
(require "interp.rkt")
(require "type-check-Cvar.rkt")
(require "utilities.rkt")
(provide (all-defined-out))


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


;; uniquify : R1 -> R1
(define (uniquify p)
  (match p
    [(Program info e) (Program info ((uniquify-exp '()) e))]))

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


;; select-instructions : C0 -> pseudo-x86
(define (select-instructions p)
  (match p
    [(CProgram info blocks)
     (X86Program info
                 (for/list ([(label tail) (in-dict blocks)])
                   (cons label (Block '() (select-instr-tail tail)))))]
;    [(CProgram info `((start . ,t)))
;     (X86Program info (list (cons 'start (Block '() (select-instr-tail t)))))]
    ))

(define (select-instr-tail t)
  (match t
    [(Seq stmt t*) 
     (append (select-instr-stmt stmt) (select-instr-tail t*))]
    [(Return (Prim 'read '()))
     (list (Callq 'read_int 0) (Jmp 'conclusion))]
    [(Return e) (append
                 (select-instr-assign (Reg 'rax) e)
                 (list (Jmp 'conclusion)))]))

(define (select-instr-atm a)
  (match a
    [(Int i) (Imm i)]
    [(Var _) a]))

(define (select-instr-stmt stmt)
  (match stmt
    [(Assign (Var x) e)
     (match e
       [(Prim '+ (list (Var x1) a2))
        #:when (equal? x x1)
        (list (Instr 'addq (list (select-instr-atm a2) (Var x))))]
       [(Prim '+ (list a1 (Var x2)))
        #:when (equal? x x2)
        (list (Instr 'addq (list (select-instr-atm a1) (Var x))))]
       [(Prim '+ (list (Int n) a2))
        (list (Instr 'movq (list (select-instr-atm a2) (Var x)))
              (Instr 'addq (list (Imm n) (Var x))))]
;       [(Prim '- (list (Var x1) (Var x2)))
;        #:when (and (equal? x x1) (equal? x x2))
;        (list (Instr 'movq 0 (Var x)))]
;       [(Prim '- (list (Var x1) a2))
;        #:when (equal? x x1)
;        (list (Instr 'subq (list (select-instr-atm a2) (Var x))))]
;       ;; TODO 要不要处理这种特殊情况？
;       [(Prim '- (list (Int n) (Var y)))
;        (list (Instr 'movq (list (Var y) (Var x)))
;              (Instr 'negq (list (Var x)))
;              (Instr 'addq (list (Imm n) (Var x))))]
       [_ (select-instr-assign (Var x) e)])]))

(define (select-instr-assign v e)
  (match e
    [(Int i)
     (list (Instr 'movq (list (select-instr-atm e) v)))]
    [(Var _)
     (list (Instr 'movq (list (select-instr-atm e) v)))]
    [(Prim 'read '())
     (list (Callq 'read_int 0)
           (Instr 'movq (list (Reg 'rax) v)))]
    [(Prim '- (list a))
     (list (Instr 'movq (list (select-instr-atm a) v))
           (Instr 'negq (list v)))]
    [(Prim '+ (list a1 a2))
     (list (Instr 'movq (list (select-instr-atm a1) v))
           (Instr 'addq (list (select-instr-atm a2) v)))]
;    [(Prim '- (list a1 a2))
;     (list (Instr 'movq (list (select-instr-atm a1) v))
;           (Instr 'subq (list (select-instr-atm a2) v)))]
    ))


;; assign-homes : pseudo-x86 -> pseudo-x86
(define (assign-homes p)
  (match p
    [(X86Program `((locals-types . ,locals-types)) blocks)
     (define locals-homes (calc-locals-homes locals-types))
     (X86Program
      `((stack-space . ,(align (* 8 (length locals-homes)) 16))
        (locals-types . ,locals-types))
      (for/list ([(label block) (in-dict blocks)])
        (cons label (assign-homes-block block locals-homes))))]))

(define (calc-locals-homes locals-types)
  (define (aux res types current-index)
    (if (null? types)
        res
        (aux (cons (cons (caar types) (Deref 'rbp (* -8 current-index)))
                   res)
             (cdr types)
             (add1 current-index))))
  (aux '() locals-types 1))

(define (assign-homes-block block locals-homes)
  (match block
    [(Block info instrs)
     (Block info
            (for/list ([instr instrs])
              (assign-homes-instr instr locals-homes)))]))

(define (assign-homes-instr instr locals-homes)
  (match instr
    [(Instr op `(,arg1 ,arg2))
     (Instr op (list (assign-homes-arg arg1 locals-homes)
                     (assign-homes-arg arg2 locals-homes)))]
    [(Instr op `(,arg))
     (Instr op (list (assign-homes-arg arg locals-homes)))]
    [_ instr]))

(define (assign-homes-arg arg locals-homes)
  (match arg
    [(Var x) (cdr (assoc x locals-homes))]
    [_ arg]))


;; patch-instructions : psuedo-x86 -> x86
(define (patch-instructions p)
  (match p
    [(X86Program info blocks)
     (X86Program
      info
      (for/list ([(label block) (in-dict blocks)])
        (cons label (patch-instr-block block))))]))

(define (patch-instr-block block)
  (match block
    [(Block info instrs)
     (Block
      info
      (append-map
       (lambda (instr)
         (match instr
           [(Instr op (list (Deref reg1 off1) (Deref reg2 off2)))
            (list (Instr 'movq (list (Deref reg1 off1) (Reg 'rax)))
                  (Instr op (list (Reg 'rax) (Deref reg2 off2))))]
           [_ (list instr)]))
       instrs))]))


;; prelude-and-conclusion : x86 -> x86
(define (prelude-and-conclusion p)
  (match p
    [(X86Program info blocks)
     (let ([stack-space (cdr (assoc 'stack-space info))])
       (X86Program
        info
        (append blocks
                (list (generate-prelude stack-space)
                      (generate-conclusion stack-space)))))]))

(define (generate-prelude stack-space)
  (cons 'main
        (Block
         '()
         (list (Instr 'pushq (list (Reg 'rbp)))
               (Instr 'movq (list (Reg 'rsp) (Reg 'rbp)))
               (Instr 'subq (list (Imm stack-space) (Reg 'rsp)))
               (Jmp 'start)))))

(define (generate-conclusion stack-space)
  (cons 'conclusion
        (Block
         '()
         (list (Instr 'addq (list (Imm stack-space) (Reg 'rsp)))
               (Instr 'popq (list (Reg 'rbp)))
               (Retq)))))

;; Define the compiler passes to be used by interp-tests and the grader
;; Note that your compiler file (the file that defines the passes)
;; must be named "compiler.rkt"
(define compiler-passes
  `(("uniquify" ,uniquify ,interp-Lvar)
    ("remove complex opera*" ,remove-complex-opera* ,interp-Lvar)
    ("explicate control" ,explicate-control ,interp-Cvar)
    ("instruction selection" ,select-instructions ,interp-pseudo-x86-0)
    ("assign homes" ,assign-homes ,interp-x86-0)
    ("patch instructions" ,patch-instructions ,interp-x86-0)
    ("prelude-and-conclusion" ,prelude-and-conclusion ,interp-x86-0)
    ))

