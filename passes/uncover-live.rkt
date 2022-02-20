#lang racket
(require "../utilities.rkt")
(provide uncover-live)

(define (uncover-live p)
  (match p
    [(X86Program info blocks)
     (X86Program
      info
      (for/list ([(label block) (in-dict blocks)])
        (cons label (uncover-live-block block))))]))

(define (uncover-live-block block)
  (match block
    [(Block info instrs)
     (Block (cons (cons 'live-after
                        (uncover-live-instrs (reverse instrs)
                                             (list (set))))
                  info)
            instrs)]))

(define (uncover-live-instrs reversed-instrs initial-sets)
  ;(display reversed-instrs)
  ;(display initial-sets)
  (match reversed-instrs
    ['() (cdr initial-sets)]
    [`(,instr . ,rst)
     ;(display 3)
     ;(display instr)
     ;(display rst)
     (define live-prior (car initial-sets))
     (uncover-live-instrs
      rst
      (cons (uncover-live-instr instr live-prior)
            initial-sets))]))

(define (uncover-live-instr instr live-prior)
  (match instr
    [(Jmp label) (dict-ref live-labels label)]
    [_ (set-union (set-subtract live-prior
                                (vars-written instr))
                  (vars-read instr))]))

(define live-labels
  `((conclusion . ,(set (Reg 'rax) (Reg 'rsp)))))

(define (vars-read instr)
  (match instr
    [(or (Instr 'addq (list arg1 arg2))
         (Instr 'subq (list arg1 arg2)))
     (set-union (arg->set arg1) (arg->set arg2))]
    [(Instr 'movq (list arg1 arg2)) (arg->set arg1)]
    [(Instr 'negq (list arg)) (arg->set arg)]
    ; TODO book p38 callq
    [(Callq label n) (set)]))

(define (vars-written instr)
  (match instr
    [(Instr op (list arg1 arg2)) (arg->set arg2)]
    [(Instr 'negq (list arg)) (arg->set arg)]
    ; TODO book p38 callq, 为什么要把caller-save放进来
    ; 如果用到了caller-save里的寄存器，在call之前把这些寄存器push一下不就行了？
    [(Callq label n) caller-save]))

(define (arg->set arg)
  (match arg
    [(Imm n) (set)]
    [(Var x) (set x)]
    [(Reg reg) (set (Reg reg))]
    [(Deref reg int) (set (Reg reg))]))
