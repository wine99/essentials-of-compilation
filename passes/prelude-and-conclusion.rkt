#lang racket
(require "../utilities.rkt")
(provide prelude-and-conclusion)

;; prelude-and-conclusion : x86 -> x86
(define (prelude-and-conclusion p)
  (match p
    [(X86Program info blocks)
     (define used-callee (dict-ref info 'used-callee))
     (define num-spilled (dict-ref info 'num-spilled))
     (define num-root-spills (dict-ref info 'num-root-spills))
     (define stack-space (+ (set-count used-callee) num-spilled))
     (define rsp-diff (if (even? stack-space)
                          (* 8 num-spilled)
                          (* 8 (+ 1 num-spilled))))
     (X86Program
        info
        (append blocks
                (list (generate-prelude used-callee rsp-diff num-root-spills)
                      (generate-conclusion used-callee rsp-diff num-root-spills))))]))

(define (generate-prelude used-callee rsp-diff num-root-spills)
  (cons 'main
        (Block
         '()
         (append
          (list (Instr 'pushq (list (Reg 'rbp)))
                (Instr 'movq (list (Reg 'rsp) (Reg 'rbp))))
          (for/list ([reg used-callee])
            (Instr 'pushq (list (Reg reg))))
          (if (= rsp-diff 0) '() (list (Instr 'subq (list (Imm rsp-diff) (Reg 'rsp)))))
          (init-gc num-root-spills)
          (list (Jmp 'start))))))

(define (generate-conclusion used-callee rsp-diff num-root-spills)
  (cons 'conclusion
        (Block
         '()
         (append
          (list (Instr 'subq (list (Imm (* 8 num-root-spills)) (Reg 'r15))))
          (if (= rsp-diff 0) '() (list (Instr 'addq (list (Imm rsp-diff) (Reg 'rsp)))))
          (reverse (for/list ([reg used-callee])
                     (Instr 'popq (list (Reg reg)))))
          (list (Instr 'popq (list (Reg 'rbp)))
                (Retq))))))

(define (init-gc num-root-spills)
  (append
   (list
    (Instr 'movq (list (Imm root-stack-size) (Reg 'rdi)))
    (Instr 'movq (list (Imm heap-size) (Reg 'rsi)))
    (Callq 'initialize 2)
    (Instr 'movq (list (Global 'rootstack_begin) (Reg 'r15))))
   (for/list ([i (in-range num-root-spills)])
     (Instr 'movq (list (Imm 0) (Deref 'r15 i))))
   (list
    (Instr 'addq (list (Imm (* 8 num-root-spills)) (Reg 'r15))))))

(define root-stack-size 16384)
(define heap-size 16384)
