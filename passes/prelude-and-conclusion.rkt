#lang racket
(require "../utilities.rkt")
(provide prelude-and-conclusion)

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
