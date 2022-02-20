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
           [(Instr op (list (Deref reg1 off1) (Deref reg2 off2)))
            (list (Instr 'movq (list (Deref reg1 off1) (Reg 'rax)))
                  (Instr op (list (Reg 'rax) (Deref reg2 off2))))]
           [_ (list instr)]))
       instrs))]))
