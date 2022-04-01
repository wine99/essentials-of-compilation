#lang racket
(require graph)
(require "../utilities.rkt")
(require "uncover-live.rkt")
(require "../graph-printing.rkt")
(provide build-interference)

(define (build-interference p)
  (match p
    [(ProgramDefs info defs)
     (ProgramDefs info (map build-interf-def defs))]))

(define (build-interf-def def)
  (match def
    [(Def f empty-params rty info blocks)
     (define interf (directed-graph '()))
     (define locals-types (dict-ref info 'locals-types))
     (define label->live (dict-ref info 'label->live))
     (for ([local-type locals-types])
       (add-vertex! interf (car local-type)))
     (for ([(label lives) (in-dict label->live)])
       (for ([live lives])
         (add-vertex! interf live)))

     (for ([(label block) (in-dict blocks)])
       (match block
         [(Block info instrs)
          (build-interf instrs (dict-ref info 'live-afters) locals-types interf)]))
     ; (print-dot interf (string-append (symbol->string f) "interf.txt"))
     (Def f empty-params rty (dict-set info 'conflicts interf) blocks)]))

(define (build-interf instrs live-afters locals-types interf)
  (for ([instr instrs] [live-after live-afters])
    (match instr
      [(Instr 'movq (list s d))
       ; d might be Var or rax, s can only be Var or Imm
       (for ([v live-after])
         (for ([d (arg->set d)]
               #:when (not (or (equal? (Var v) s) (equal? v d))))
           (verbose (format "~a -- ~a\n" v d))
           (add-edge! interf v d)))]
      [(Instr 'movzbq (list (ByteReg s) d))
       (for ([v live-after])
         (for ([d (arg->set d)]
               #:when (not (equal? (byte-reg->full-reg s) d)))
           (verbose (format "~a -- ~a\n" v d))
           (add-edge! interf v d)))]
      [(or (Callq _ _) (IndirectCallq _ _))
       (for ([v live-after])
         (if (root-type? (dict-ref locals-types v #f))
             (begin
               (for ([d registers-for-alloc])
                 (add-edge! interf v d))
               (for ([d live-after]
                     #:when (not (root-type? (dict-ref locals-types d #f))))
                 ; Vars that will be put on the root stack should not have
                 ; same color as vars that are going the regular stack
                 (add-edge! interf v d)))
             (for ([d (caller-save-for-alloc)])
               (add-edge! interf v d))))]
      [_
       (for ([v live-after])
         (for ([d (vars-written instr)]
               #:when (not (equal? v d)))
           (verbose (format "~a -- ~a\n" v d))
           (add-edge! interf v d)))])))
