#lang racket
(require "../utilities.rkt")
(provide select-instructions)

;; select-instructions : C0 -> pseudo-x86
(define (select-instructions p)
  (match p
    [(ProgramDefs info defs)
     (ProgramDefs info (map select-instr-def defs))]))

(define (select-instr-def def)
  (match def
    [(Def name `([,xs : ,ts] ...) rty info blocks)
     (define new-blocks
       (for/list ([(label tail) (in-dict blocks)])
         (cons label (Block '() (select-instr-tail name tail)))))
     (define param-moves
       (for/list ([x xs] [r arg-registers])
         (Instr 'movq (list (Reg r) (Var x)))))
     (define start-label (symbol-append name 'start))
     (define new-start-block
       (match (dict-ref new-blocks start-label)
         [(Block info instrs)
          (Block info (append param-moves instrs))]))
     (define new-info
       (dict-set-all
        info
        `((locals-types . ,(append (map cons xs ts)
                                  (dict-ref info 'locals-types)))
          (num-params . ,(length xs)))))
     (Def name '() rty new-info
          (dict-set new-blocks start-label new-start-block))]))

(define (select-instr-tail def-name t)
  (match t
    [(Seq stmt t*)
     (append (select-instr-stmt stmt) (select-instr-tail def-name t*))]
    [(Return (Prim 'read '()))
     (list (Callq 'read_int 0)
           (Jmp (symbol-append def-name 'conclusion)))]
    [(Return e) (append
                 (select-instr-assign (Reg 'rax) e)
                 (list (Jmp (symbol-append def-name 'conclusion))))]
    [(Goto label) (list (Jmp label))]
    [(IfStmt (Prim cmp (list a1 a2))
             (Goto label1)
             (Goto label2))
     #:when (cmp? cmp)
     (list (Instr 'cmpq (list (select-instr-atm a2) (select-instr-atm a1)))
           (JmpIf (cmp->suffix cmp) label1)
           (Jmp label2))]
    [(IfStmt (Prim 'vector-ref args)
             (Goto label1)
             (Goto label2))
     (append (select-instr-assign (Reg 'rax) (Prim 'vector-ref args))
             (list (Instr 'cmpq (list (Reg 'rax) (Imm 1)))
                   (JmpIf 'e label1)
                   (Jmp label2)))]
    [(TailCall f args)
     (append (for/list ([arg (map select-instr-atm args)] [r arg-registers])
               (Instr 'movq (list arg (Reg r))))
             (list (TailJmp f (length args))))]))

(define (select-instr-atm a)
  (match a
    [(Int i) (Imm i)]
    [(Bool #t) (Imm 1)]
    [(Bool #f) (Imm 0)]
    [(Var x) (Var x)]
    [(Void) (Imm 0)]))

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

       [(Call f args)
        (append (for/list ([arg (map select-instr-atm args)] [r arg-registers])
                  (Instr 'movq (list arg (Reg r))))
                (list (IndirectCallq f (length args))
                      (Instr 'movq (list (Reg 'rax) (Var x)))))]
       [_ (select-instr-assign (Var x) e)])]

    [(Prim 'read '()) (list (Callq 'read_int 0))]
    [(Prim 'vector-set! (list vec (Int n) rhs))
     (list (Instr 'movq (list (select-instr-atm vec) (Reg 'r11)))
           (Instr 'movq (list (select-instr-atm rhs) (Deref 'r11 (* 8 (add1 n))))))]
    [(Collect bytes)
     (list (Instr 'movq (list (Reg 'r15) (Reg 'rdi)))
           (Instr 'movq (list (Imm bytes) (Reg 'rsi)))
           (Callq 'collect 2))]

    [(Call f args)
     (append (for/list ([arg (map select-instr-atm args)] [r arg-registers])
               (Instr 'movq (list arg (Reg r))))
             (list (IndirectCallq f (length args))))]))


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
         (list (Instr 'xorq (list (Imm 1) v)))
         (list (Instr 'movq (list (select-instr-atm a) v))
               (Instr 'xorq (list (Imm 1) v))))]
    [(Prim cmp (list a1 a2))
     #:when (cmp? cmp)
     (list (Instr 'cmpq (list (select-instr-atm a2) (select-instr-atm a1)))
           (Instr 'set (list (cmp->suffix cmp) (ByteReg 'al)))
           (Instr 'movzbq (list (ByteReg 'al) v)))]

    [(Prim 'vector-length (list vec))
     (list (Instr 'movq (list (select-instr-atm vec) (Reg 'r11))) 
           (Instr 'movq (list (Deref 'r11 0) (Reg 'rax)))
           (Instr 'sarq (list (Imm 1) (Reg 'rax)))
           (Instr 'andq (list (Imm 63) (Reg 'rax)))
           (Instr 'movq (list (Reg 'rax) v)))]
    [(Prim 'vector-ref (list vec (Int n)))
     (list (Instr 'movq (list (select-instr-atm vec) (Reg 'r11))) 
           (Instr 'movq (list (Deref 'r11 (* 8 (add1 n))) v)))]
    [(Prim 'vector-set! (list vec (Int n) rhs))
     (list (Instr 'movq (list (select-instr-atm vec) (Reg 'r11)))
           (Instr 'movq (list (select-instr-atm rhs) (Deref 'r11 (* 8 (add1 n)))))
           (Instr 'movq (list (Imm 0) v)))]
    [(GlobalValue x) (list (Instr 'movq (list (Global x) v)))]
    [(Allocate len types)
     (define tag (calculate-tag (cdr types) len))
     (list (Instr 'movq (list (Global 'free_ptr) (Reg 'r11)))
           (Instr 'addq (list (Imm (* 8 (add1 len))) (Global 'free_ptr)))
           (Instr 'movq (list (Imm tag) (Deref 'r11 0)))
           (Instr 'movq (list (Reg 'r11) v)))]

    [(FunRef _ _)
     (list (Instr 'leaq (list e v)))]))


(define cmp-suffix #hash((eq? . e)
                         (< . l)
                         (<= . le)
                         (> . g)
                         (>= . ge)))
(define (cmp? op) (hash-has-key? cmp-suffix op))
(define (cmp->suffix op) (hash-ref cmp-suffix op))

(define (calculate-tag types len)
  ;;highest 7 bits are unused
  ;;lowest 1 bit is 0 saying this is not a forwarding pointer
  (define is-not-forward-tag 1)
  ;;next 6 lowest bits are the length
  (define length-tag (arithmetic-shift len 1))
  ;;bits [6,56] are a bitmask indicating if [0,50] are pointers
  (define ptr-tag
    (for/fold ([tag 0]) ([t (in-list types)] [i (in-naturals 7)])
      (if (root-type? t)
          (bitwise-ior tag (arithmetic-shift 1 i))
          tag)))
  ;;combine the tags into a single quad word
  (bitwise-ior ptr-tag length-tag is-not-forward-tag))
