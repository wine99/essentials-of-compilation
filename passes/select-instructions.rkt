#lang racket
(require "../utilities.rkt")
(provide select-instructions)

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
;       ;; TODO 要不要处理这种特殊情况?
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