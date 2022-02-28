#lang racket
(require graph)
(require "../utilities.rkt")
(require "../priority_queue.rkt")
(require "assign-homes.rkt")
(provide allocate-registers)

(define (allocate-registers p)
  (match p
    [(X86Program info blocks)
     (define vars
       (for/list ([(var type) (in-dict (dict-ref info 'locals-types))]) var))
     (define interf (dict-ref info 'conflicts))
     (define-values (vars-colors largest-color) (color-graph interf vars))
     ; TODO used-caller-save-regs 要统计吗？是否应该在call指令前把它们push到栈上
     (define u-c-r (used-callee-regs vars-colors))
     (define num-u-c-r (set-count u-c-r))
     (define num-r-f-a (num-registers-for-alloc))
     (define locals-homes
       (for/hash ([var vars])
         (define home (color->home (hash-ref vars-colors var) num-r-f-a num-u-c-r))
         (verbose (format "home of ~a is ~a" var home))
         (values var home)))
     (X86Program
      (dict-set info 'used-callee u-c-r)
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
    (verbose (format "color ~a to ~a" var color))
    (hash-set! vars-colors var color)
    (for ([neighbor (in-neighbors interf var)]
          #:when (not (set-member? registers neighbor)))
      (hash-set! unavail-colors
                 neighbor (set-add (hash-ref unavail-colors neighbor) color))
      ; TODO read about heap decrease key
      (pqueue-decrease-key! Q (hash-ref vars-nodes neighbor))))

  (values vars-colors largest-color))


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
       (set-member? callee-save (color->register color))))

(define (color->home color num-registers-for-alloc num-used-callee-regs)
  (if (< color num-registers-for-alloc)
      (Reg (color->register color))
      (Deref 'rbp
             (* -8 (+ num-used-callee-regs
                       (- color num-registers-for-alloc))))))
