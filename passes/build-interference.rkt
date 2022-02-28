#lang racket
(require graph)
(require "../utilities.rkt")
(require "uncover-live.rkt")
(provide build-interference)

(define (build-interference p)
  (match p
    [(X86Program info (list (cons 'start block)))
     (define interf (directed-graph '()))
     (define locals-types (dict-ref info 'locals-types))
     (for ([local-type locals-types])
       (add-vertex! interf (car local-type)))
     (for ([(label lives) (in-dict live-labels)])
       (for ([live lives])
         (add-vertex! interf live)))
     (match block
       [(Block info instrs)
        (build-interf instrs (dict-ref info 'live-afters) interf)])
     ; (verbose (graphviz interf))
     (X86Program (dict-set info 'conflicts interf)
                 (list (cons 'start block)))]))

(define (build-interf instrs live-afters interf)
  (for ([instr instrs] [live-after live-afters])
    (match instr
      [(Instr 'movq (list s d))
       ; d可能是Var可能是rax，s只能是Var或者Imm
       (for ([v live-after])
         (for ([d (arg->set d)]
               #:when (not (or (equal? (Var v) s) (equal? v d))))
           (verbose (format "~a -- ~a\n" v d))
           (add-edge! interf v d)))]
      [_
       (for ([v live-after])
         (for ([d (vars-written instr)]
               #:when (not (equal? v d)))
           (verbose (format "~a -- ~a\n" v d))
           (add-edge! interf v d)))])))
