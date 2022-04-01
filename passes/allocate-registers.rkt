#lang racket
(require graph)
(require "../utilities.rkt")
(require "../priority_queue.rkt")
(require "assign-homes.rkt")
(provide allocate-registers)

(define (allocate-registers p)
  (match p
    [(ProgramDefs info defs)
     (ProgramDefs info (map alloc-reg-def defs))]))

(define (alloc-reg-def def)
  (match def
    [(Def f empty-params rty info blocks)
     (define vars
       (for/list ([(var type) (in-dict (dict-ref info 'locals-types))]) var))
     (define interf (dict-ref info 'conflicts))

     (define-values (_vars-colors largest-color) (color-graph interf vars))
     (define-values (vars-colors num-spilled num-root-spills)
       (recolor _vars-colors (dict-ref info 'locals-types) largest-color))
     (define used-callee (used-callee-regs vars-colors))

     (define locals-homes
       (for/hash ([var vars])
         (define home (color->home (hash-ref vars-colors var)
                                   (num-registers-for-alloc)
                                   (set-count used-callee)
                                   num-root-spills))
         (verbose (format "home of ~a is ~a\n" var home))
         (values var home)))
     (Def f empty-params rty
          `((used-callee . ,used-callee)
            (num-spilled . ,num-spilled)
            (num-root-spills . ,num-root-spills)
            (num-params . ,(dict-ref info 'num-params)))
          (for/list ([(label block) (in-dict blocks)])
            (cons label (assign-homes-block block locals-homes))))]))


; takes an interference graph and a list of all the variables
; return a mapping of variables to their colors (numbers)
(define (color-graph interf vars)
  (define largest-color 0)
  ; map var to colors that cannot be assigned
  (define unavail-colors (make-hash))
  (define Q (make-pqueue
             (lambda (u v)
               (>= (set-count (hash-ref unavail-colors u))
                   (set-count (hash-ref unavail-colors v))))))
  ; map var to pq-node
  (define vars-nodes (make-hash))
  ; map var to color
  (define vars-colors (make-hash))

  (for ([var vars])
    (define adj-regs
      (for/list ([location (in-neighbors interf var)]
                 #:when (set-member? registers location))
        location))
    (hash-set! unavail-colors var (list->set (map register->color adj-regs)))
    (hash-set! vars-nodes var (pqueue-push! Q var)))

  (while (> (pqueue-count Q) 0)
    (define var (pqueue-pop! Q))
    (define color (choose-color var (hash-ref unavail-colors var)))
    (cond [(> color largest-color)
           (set! largest-color color)])
    (verbose (format "color ~a to ~a\n" var color))
    (hash-set! vars-colors var color)
    (for ([neighbor (in-neighbors interf var)]
          #:when (not (set-member? registers neighbor)))
      (hash-set! unavail-colors
                 neighbor (set-add (hash-ref unavail-colors neighbor) color))
      ; TODO read about heap decrease key
      (pqueue-decrease-key! Q (hash-ref vars-nodes neighbor))))

  (values vars-colors largest-color))


; Collect colors of spilled root type vars and
; compact those vars to a continuous sequence of smaller colors
(define (recolor vars-colors vars-types largest-color)
  (define smallest-spilled-color (num-registers-for-alloc))
  (when (largest-color . < . smallest-spilled-color)
    (values vars-colors 0 0))

  (define spilled-colors (range smallest-spilled-color (+ largest-color 1)))
  (define spilled-root-colors
    (for/set ([(var color) (in-dict vars-colors)]
              #:when (and (color . >= . smallest-spilled-color)
                          (root-type? (dict-ref vars-types var))))
      color))
  (define spilled-not-root-colors
    (for/list ([color spilled-colors]
               #:when (not (set-member? spilled-root-colors color)))
      color))

  (define mapping
    (for/hash ([color spilled-colors]
               [new-color (append (set->list spilled-root-colors)
                                  spilled-not-root-colors)])
      (values color new-color)))

  (define new-vars-colors
    (for/hash ([(var color) (in-dict vars-colors)])
      (if (color . >= . smallest-spilled-color)
          (values var (hash-ref mapping color))
          (values var color))))
  (define num-spilled (length spilled-not-root-colors))
  (define num-root-spills (set-count spilled-root-colors))
  (values new-vars-colors num-spilled num-root-spills))


(define (choose-color var unavail-colors)
  (for/first ([c (in-naturals)]
              #:when (color-avail? unavail-colors c))
    c))

(define (color-avail? unavail-colors color)
  (not (set-member? unavail-colors color)))

(define (used-callee-regs vars-colors)
  (for/set ([(var color) vars-colors]
            #:when (callee-color? color))
    (color->register color)))

(define (callee-color? color)
  (and (< color (num-registers-for-alloc))
       (set-member? (callee-save-for-alloc) (color->register color))))

(define (color->home color num-reg-for-alloc num-used-callee num-root-spills)
  (cond
    [(< color num-reg-for-alloc)
     (Reg (color->register color))]
    [(< color (+ num-reg-for-alloc num-root-spills))
     (Deref 'r15 (* -8 (add1 (- color num-reg-for-alloc))))]
    [else
     (Deref 'rbp (* -8
                    (+ (add1 (- color num-reg-for-alloc num-root-spills))
                       num-used-callee)))]))
