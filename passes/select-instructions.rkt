#lang racket
(require "../utilities.rkt")
(provide select-instructions)

;; select-instructions : C0 -> pseudo-x86
(define (select-instructions p)
  (match p
    [(CProgram info blocks)
     (X86Program info
                 (for/list ([(label tail) (in-dict blocks)])
                   (cons label (Block '() (select-instr-tail tail)))))]))

(define (select-instr-tail t)
  (match t
    [(Seq stmt t*) 
     (append (select-instr-stmt stmt) (select-instr-tail t*))]
    [(Return (Prim 'read '()))
     (list (Callq 'read_int 0) (Jmp 'conclusion))]
    [(Return e) (append
                 (select-instr-assign (Reg 'rax) e)
                 (list (Jmp 'conclusion)))]
    [(Goto label) (list (Jmp label))]
    [(IfStmt (Prim cmp (list a1  a2))
             (Goto label1)
             (Goto label2))
     #:when (cmp? cmp)
     (list (Instr 'cmpq (list (select-instr-atm a2) (select-instr-atm a1)))
           (JmpIf (cmp->suffix cmp) label1)
           (Jmp label2))]))

(define (select-instr-atm a)
  (match a
    [(Int i) (Imm i)]
    [(Bool #t) (Imm 1)]
    [(Bool #f) (Imm 0)]
    [(Var x) (Var x)]))

; According to definition, "stmt ::= var = exp;"
; It is better to not having two vars in a binary operation
; since they may both be allocated to a stack location
(define (select-instr-stmt stmt)
  (match stmt
    [(Assign (Var x) e)
     (match e
       [(Prim '+ (list (Var x1) a2))
        #:when (equal? x x1)
        (list (Instr 'addq (list (select-instr-atm a2) (Var x))))]
       [(Prim '+ (list a1 (Var x1)))
        #:when (equal? x x1)
        (list (Instr 'addq (list (select-instr-atm a1) (Var x))))]
       [(Prim '+ (list (Int n) a2))
        (list (Instr 'movq (list (select-instr-atm a2) (Var x)))
              (Instr 'addq (list (Imm n) (Var x))))]
       [(Prim '- (list (Var x1) a2))
        #:when (equal? x x1)
        (list (Instr 'subq (list (select-instr-atm a2) (Var x))))]
       [(Prim '- (list a1 (Var x1)))
        #:when (equal? x x1)
        (list (Instr 'negq (list (Var x)))
              (Instr 'addq (list (select-instr-atm a1) (Var x))))]
       [_ (select-instr-assign (Var x) e)])]))

; This v can also be (Reg 'rax)
(define (select-instr-assign v e)
  (match e
    [(or (Int _) (Var _) (Bool _))
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
    [(Prim '- (list a1 a2))
     (list (Instr 'movq (list (select-instr-atm a1) v))
           (Instr 'subq (list (select-instr-atm a2) v)))]

    [(Prim 'not (list a))
     (if (equal? v a)
         (list (Instr 'xorq (Imm 1) v))
         (list (Instr 'movq (select-instr-atm a) v)
               (Instr 'xorq (Imm 1) v)))]
    [(Prim cmp (list a1 a2))
     #:when (cmp? cmp)
     (list (Instr 'cmpq (list (select-instr-atm a2) (select-instr-atm a1)))
           (Instr 'set (list (cmp->suffix cmp) (ByteReg 'al)))
           (Instr 'movzbq (list (ByteReg 'al) v)))]))

(define cmp-suffix #hash((eq? . e)
                         (< . l)
                         (<= . le)
                         (> . g)
                         (>= . ge)))
(define (cmp? op) (hash-has-key? cmp-suffix op))
(define (cmp->suffix op) (hash-ref cmp-suffix op))
