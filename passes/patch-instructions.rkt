#lang racket
(require "../utilities.rkt")
(provide patch-instructions)

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
           [(Instr 'movq (list (Deref reg1 off1) (Deref reg2 off2)))
            #:when (and (eq? reg1 reg2) (eq? off1 off2))
            '()]
           [(Instr 'movq (list (Reg reg1) (Reg reg2)))
            #:when (eq? reg1 reg2)
            '()]
           [(Instr 'movzbq (list a1 a2))
            #:when (not (Reg? a2))
            (list (Instr 'movzbq (list a1 (Reg 'rax)))
                  (Instr 'movq (list (Reg 'rax) a2)))]
           [(Instr 'cmpq (list a1 (Imm n)))
            (list (Instr 'movq (list (Imm n) (Reg 'rax)))
                  (Instr 'cmpq (list a1 (Reg 'rax))))]
           [(Instr op (list (Deref reg1 off1) (Deref reg2 off2)))
            (list (Instr 'movq (list (Deref reg1 off1) (Reg 'rax)))
                  (Instr op (list (Reg 'rax) (Deref reg2 off2))))]
           [_ (list instr)]))
       instrs))]))
