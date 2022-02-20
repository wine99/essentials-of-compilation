#lang racket
(require "../utilities.rkt")
(provide uniquify)

;; uniquify : R1 -> R1
(define (uniquify p)
  (match p
    [(Program info e) (Program info ((uniquify-exp '()) e))]))

(define (uniquify-exp env)
  (lambda (e)
    (match e
      [(Var x)
       (let ([p (assoc x env)])
         (if (and p (eq? (car p) x))
             (Var (cdr p))
             ;; TODO maybe check somewhere else
             (error 'syntax-error "unbound identifier " x)))]
      ;; TODO 重新构造 (Int n) 更高效还是直接返回 e ?
      [(Int n) (Int n)]
      [(Let x e body)
       (let ([new-x (gensym)])
         (Let new-x
              ((uniquify-exp env) e)
              ((uniquify-exp (cons (cons x new-x) env)) body)))]
      [(Prim op es)
       (Prim op (for/list ([e es]) ((uniquify-exp env) e)))])))
