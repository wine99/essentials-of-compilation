#lang racket
(require "../utilities.rkt")
(provide assign-homes)

;; assign-homes : pseudo-x86 -> pseudo-x86
(define (assign-homes p)
  (match p
    [(X86Program `((locals-types . ,locals-types)) blocks)
     (define locals-homes (calc-locals-homes locals-types))
     (X86Program
      `((stack-space . ,(align (* 8 (length locals-homes)) 16))
        (locals-types . ,locals-types))
      (for/list ([(label block) (in-dict blocks)])
        (cons label (assign-homes-block block locals-homes))))]))

(define (calc-locals-homes locals-types)
  (define (aux res types current-index)
    (if (null? types)
        res
        (aux (cons (cons (caar types) (Deref 'rbp (* -8 current-index)))
                   res)
             (cdr types)
             (add1 current-index))))
  (aux '() locals-types 1))

(define (assign-homes-block block locals-homes)
  (match block
    [(Block info instrs)
     (Block info
            (for/list ([instr instrs])
              (assign-homes-instr instr locals-homes)))]))

(define (assign-homes-instr instr locals-homes)
  (match instr
    [(Instr op `(,arg1 ,arg2))
     (Instr op (list (assign-homes-arg arg1 locals-homes)
                     (assign-homes-arg arg2 locals-homes)))]
    [(Instr op `(,arg))
     (Instr op (list (assign-homes-arg arg locals-homes)))]
    [_ instr]))

(define (assign-homes-arg arg locals-homes)
  (match arg
    [(Var x) (cdr (assoc x locals-homes))]
    [_ arg]))