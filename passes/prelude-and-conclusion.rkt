#lang racket
(require "../utilities.rkt")
(provide prelude-and-conclusion)

;; prelude-and-conclusion : x86 -> x86
(define (prelude-and-conclusion p)
  (match p
    [(X86Program info blocks)
     (define used-callee (dict-ref info 'used-callee))
     (define num-spilled (dict-ref info 'num-spilled))
     (define stack-space (+ (set-count used-callee) num-spilled))
     (define rsp-diff (if (even? stack-space)
                          (* 8 num-spilled)
                          (* 8 (+ 1 num-spilled))))
     (X86Program
        info
        (append blocks
                (list (generate-prelude used-callee rsp-diff)
                      (generate-conclusion used-callee rsp-diff))))]))

(define (generate-prelude used-callee rsp-diff)
  (cons 'main
        (Block
         '()
         (append
          (list (Instr 'pushq (list (Reg 'rbp)))
                (Instr 'movq (list (Reg 'rsp) (Reg 'rbp))))
          (for/list ([reg used-callee])
            (Instr 'pushq (list (Reg reg))))
          (if (= rsp-diff 0) '() (list (Instr 'subq (list (Imm rsp-diff) (Reg 'rsp)))))
          (list (Jmp 'start))))))

(define (generate-conclusion used-callee rsp-diff)
  (cons 'conclusion
        (Block
         '()
         (append
          (if (= rsp-diff 0) '() (list (Instr 'addq (list (Imm rsp-diff) (Reg 'rsp)))))
          (reverse (for/list ([reg used-callee])
                     (Instr 'popq (list (Reg reg)))))
          (list (Instr 'popq (list (Reg 'rbp)))
                (Retq))))))
