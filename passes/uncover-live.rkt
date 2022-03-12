#lang racket
(require graph)
(require "../utilities.rkt")
(provide uncover-live vars-read vars-written arg->set)

(define (uncover-live p)
  (match p
    [(X86Program info blocks)
     (define G (blocks->graph blocks))
     (define new-blocks (make-hash))
     (define label->live (make-hash))
     (hash-set! label->live 'conclusion (set 'rax 'rsp))
     
     (for ([label (tsort (transpose G))])
       (define block (dict-ref blocks label))
       (define-values (new-block live-before)
         (uncover-live-block block label->live))
       (hash-set! new-blocks label new-block)
       (hash-set! label->live label live-before))
     
     (X86Program
      (dict-set info 'label->live label->live)
      ; keep the original order of the blocks
      (for/list ([(label _) (in-dict blocks)])
        (cons label (hash-ref new-blocks label))))]))

(define (blocks->graph blocks)
  (define G (directed-graph '()))
  (for ([label (in-dict-keys blocks)])
    (add-vertex! G label))
  (for ([(s b) (in-dict blocks)])
    (for ([t (adjacent-block-labels b)])
      (add-directed-edge! G s t)))
  G)

(define (adjacent-block-labels b)
  (match b
    [(Block info instrs)
     (for/fold ([adj (set)])
               ([instr instrs])
       (match instr
         [(Jmp 'conclusion) adj]
         [(Jmp label) (set-add adj label)]
         [(JmpIf _ label) (set-add adj label)]
         [_ adj]))]))

(define (uncover-live-block block label->live)
  (match block
    [(Block info instrs)
     (define lives (uncover-live-instrs (reverse instrs)
                                        (list (set))
                                        label->live))
     (values
      (Block (dict-set info 'live-afters (cdr lives))
             instrs)
      (car lives))]))

(define (uncover-live-instrs reversed-instrs initial-sets label->live)
  (match reversed-instrs
    ['() initial-sets]
    [`(,instr . ,rst)
     (define live-prior (car initial-sets))
     (uncover-live-instrs
      rst
      (cons (uncover-live-instr instr live-prior label->live)
            initial-sets)
      label->live)]))

(define (uncover-live-instr instr live-prior label->live)
  (match instr
    [(Jmp label) (dict-ref label->live label)]
    [(JmpIf cc label)
     (set-union live-prior (dict-ref label->live label))]
    [_ (set-union (set-subtract live-prior
                                (vars-written instr))
                  (vars-read instr))]))

(define (vars-read instr)
  (match instr
    [(or (Instr 'addq (list arg1 arg2))
         (Instr 'subq (list arg1 arg2))
         (Instr 'xorq (list arg1 arg2))
         (Instr 'cmpq (list arg1 arg2)))
     (set-union (arg->set arg1) (arg->set arg2))]
    [(or (Instr 'movq (list arg1 arg2))
         (Instr 'movzbq (list arg1 arg2)))
     (arg->set arg1)]
    [(or (Instr 'negq (list arg))
         (Instr 'set (list _ arg)))
     (arg->set arg)]
    [(Callq label n) (set)]
    ; jmp is handled in uncover-live-instr
    [(Jmp _) (set)]
    [(JmpIf _ _) (set)]))

(define (vars-written instr)
  (match instr
    [(Instr 'cmpq _) (set)]
    [(Instr op (list arg1 arg2)) (arg->set arg2)]
    [(Instr 'negq (list arg)) (arg->set arg)]
    [(Instr 'set (list _ arg)) (arg->set arg)]
    [(Callq label n) caller-save]
    ; jmp is handled in uncover-live-instr
    [(Jmp _) (set)]
    [(JmpIf _ _) (set)]))

(define (arg->set arg)
  (match arg
    [(Imm n) (set)]
    [(Var x) (set x)]
    [(Reg reg) (set reg)]
    [(ByteReg reg) (set (byte-reg->full-reg reg))]
    [(Deref reg int) (set reg)]))
