#lang racket
(require "../utilities.rkt")
(provide prelude-and-conclusion)

;; prelude-and-conclusion : x86 -> x86
(define (prelude-and-conclusion p)
  (match p
    [(ProgramDefs info defs)
     (define new-defs (map pre-and-con-def defs))
     (X86Program info (append-map Def-body new-defs))]))

(define (pre-and-con-def def)
  (match def
    [(Def f empty-params rty info blocks)
     (define used-callee (dict-ref info 'used-callee))
     (define num-spilled (dict-ref info 'num-spilled))
     (define num-root-spills (dict-ref info 'num-root-spills))
     (define stack-space (+ (set-count used-callee) num-spilled))
     (define rsp-diff (if (even? stack-space)
                          (* 8 num-spilled)
                          (* 8 (+ 1 num-spilled))))
     
     (define new-blocks
       (for/list ([(label block) (in-dict blocks)])
         (cons label
               (expand-tail-jmp block used-callee rsp-diff num-root-spills))))
     (Def f empty-params rty info
          (append 
           new-blocks
           (list (generate-prelude f used-callee rsp-diff num-root-spills)
                 (generate-conclusion f used-callee rsp-diff num-root-spills))))]))


(define (expand-tail-jmp block used-callee rsp-diff num-root-spills)
  (match block
    [(Block info instrs)
     (Block
      info
      (append*
       (for/list ([instr instrs])
         (match instr
           [(TailJmp arg arity)
            (append (conclusion-instrs used-callee rsp-diff num-root-spills)
                    (list (IndirectJmp arg)))]
           [(Instr 'leaq (list (FunRef name arity) arg2))
            (list (Instr 'leaq (list (Global name) arg2)))]
           [_ (list instr)]))))]))

(define (generate-prelude f used-callee rsp-diff num-root-spills)
  (cons
   f
   (Block
    '()
    (append
     (list (Instr 'pushq (list (Reg 'rbp)))
           (Instr 'movq (list (Reg 'rsp) (Reg 'rbp))))
     (for/list ([reg used-callee])
       (Instr 'pushq (list (Reg reg))))
     (if (= rsp-diff 0) '() (list (Instr 'subq (list (Imm rsp-diff) (Reg 'rsp)))))
     (if (eq? f 'main) (init-gc) '())
     (set-root-stack-frame num-root-spills)
     (list (Jmp (symbol-append f 'start)))))))

(define (generate-conclusion f used-callee rsp-diff num-root-spills)
  (cons (symbol-append f 'conclusion)
        (Block '()
               (append (conclusion-instrs used-callee rsp-diff num-root-spills)
                       (list (Retq))))))

(define (conclusion-instrs used-callee rsp-diff num-root-spills)
  (append
   (if (= num-root-spills 0) '()
       (list (Instr 'subq (list (Imm (* 8 num-root-spills)) (Reg 'r15)))))
   (if (= rsp-diff 0) '()
       (list (Instr 'addq (list (Imm rsp-diff) (Reg 'rsp)))))
   (reverse (for/list ([reg used-callee])
              (Instr 'popq (list (Reg reg)))))
   (list (Instr 'popq (list (Reg 'rbp))))))

(define (init-gc)
  (list
    (Instr 'movq (list (Imm root-stack-size) (Reg 'rdi)))
    (Instr 'movq (list (Imm heap-size) (Reg 'rsi)))
    (Callq 'initialize 2)
    (Instr 'movq (list (Global 'rootstack_begin) (Reg 'r15)))))

(define (set-root-stack-frame num-root-spills)
  (append
   (for/list ([i (in-range num-root-spills)])
     (Instr 'movq (list (Imm 0) (Deref 'r15 (* 8 i)))))
   (if (= num-root-spills 0) '()
       (list (Instr 'addq (list (Imm (* 8 num-root-spills)) (Reg 'r15)))))))

(define root-stack-size 16384)
;(define heap-size 16384)
(define heap-size 16)
