#lang racket
(require "../utilities.rkt")
(require "../type-check-Llambda.rkt")
(require "convert-assignments.rkt")
(provide convert-to-closures)

(define (convert-to-closures p)
  (typed-vars false)
  (match p
    [(ProgramDefs info defs)
     (define-values (new-defs listof-clo-defs)
       (for/lists (l1 l2)
                  ([def defs])
         (match def
           [(Def name (list `[,xs : ,ts] ...) rty info body)
            (define vars-types (collect-vars-types body))
            (define-values (new-body clo-defs)
              ((convert-exp vars-types) body))
            (define param*^
              (for/list ([x xs] [t ts]) `[,x : ,(convert-type t)]))
            (unless (eq? name 'main)
              (set! param*^ (cons `[,(gensym 'clos) : (Vector _)] param*^)))
            (values
             (Def name param*^ (convert-type rty) info new-body)
             clo-defs)])))
     (ProgramDefs info (append* new-defs listof-clo-defs))]))

(define (collect-vars-types exp)
  (define recur collect-vars-types)
  (match exp
    [(or (Int _) (Bool _) (Void) (FunRef _ _)) (hash)]
    [(SetBang _ e) (recur e)]
    [(HasType (or (Var x) (GetBang x)) type)
     (hash x type)]
    [(Let x e body) (hash-union (recur e) (recur body))]
    [(Prim op '()) (hash)]
    [(Prim op es) (apply hash-union (map recur es))]
    [(If e1 e2 e3) (hash-union (recur e1) (recur e2) (recur e3))]
    [(Begin es final-e)
     (apply hash-union (map recur (append es (list final-e))))]
    [(WhileLoop cnd body) (hash-union (recur cnd) (recur body))]
    [(HasType e type) (recur e)]
    [(Apply fun args) (apply hash-union (map recur (append (list fun) args)))]
    [(Lambda param* rty body) (collect-vars-types body)]))


(define ((convert-exp vars-types) exp)
  (define recur (convert-exp vars-types))
  (match exp
    [(or (Int _) (Bool _) (Void)) (values exp '())]
    ; TODO do we still HasType afterward?
    [(HasType (Var x) type)
     #;(values (HasType (Var x) (convert-type type)) '())
     (values (Var x) '())]
    [(HasType (GetBang x) type)
     #;(values (HasType (GetBang x) (convert-type type)) '())
     (values (GetBang x) '())]
    [(Let x e body)
     (define-values (e^ ds1) (recur e))
     (define-values (body^ ds2) (recur body))
     (values (Let x e^ body^) (append ds1 ds2))]
    [(Prim op es)
     (define-values (es^ dss) (for/lists (l1 l2) ([e es]) (recur e)))
     (values (Prim op es^) (append* dss))]
    [(If e1 e2 e3)
     (define-values (e1^ ds1) (recur e1))
     (define-values (e2^ ds2) (recur e2))
     (define-values (e3^ ds3) (recur e3))
     (values (If e1^ e2^ e3^) (append ds1 ds2 ds3))]
    [(SetBang x e)
     (define-values (e^ ds) (recur e))
     (values (SetBang x e^) ds)]
    [(Begin es final-e)
     (define-values (es^ dss) (for/lists (l1 l2) ([e es]) (recur e)))
     (define ds1 (append* dss))
     (define-values (final-e^ ds2) (recur final-e))
     (values (Begin es^ final-e^) (append ds1 ds2))]
    [(WhileLoop cnd body)
     (define-values (cnd^ ds1) (recur cnd))
     (define-values (body^ ds2) (recur body))
     (values (WhileLoop cnd^ body^) (append ds1 ds2))]
    [(HasType e type)
     (define-values (e^ ds) (recur e))
     (values (HasType e^ type) ds)]
    [(FunRef name arity)
     (values (Closure arity (list (FunRef name arity))) '())]
    [(Apply fun args)
     (define-values (fun^ ds) (recur fun))
     (define-values (args^ dss) (for/lists (l1 l2) ([a args]) (recur a)))
     (define tmp (gensym 'clos))
     (values
      (if (or (Var? fun^) (GetBang? fun^))
          (Apply (Prim 'vector-ref (list fun^ (Int 0)))
                 (cons fun^ args^))
          (Let tmp fun^
               (Apply (Prim 'vector-ref (list (Var tmp) (Int 0)))
                      (cons (Var tmp) args^))))
      (append* ds dss))]
    [(Lambda (list `[,xs : ,ts] ...) rty body)
     (define-values (body^ ds) (recur body))
     (define name (gensym 'lambda))
     (define frees (set->list (free-vars exp)))
     (define frees-types
       (foldr (lambda (x frees-types)
                (cons (convert-type (hash-ref vars-types x)) frees-types))
              '()
              frees))
     (define param*^ (for/list ([x xs] [t ts]) `[,x : ,(convert-type t)]))
     (define clos (gensym 'clos))
     (values (Closure (length xs)
                      (cons (FunRef name (length xs))
                            (map Var frees)))
             (cons (Def name
                        `([,clos : (Vector _ ,@frees-types)] ,@param*^)
                        (convert-type rty)
                        '()
                        (convert-closure-body body^ frees clos))
                   ds))]))

(define (convert-closure-body new-body free-vars clos-vec-name)
  (make-lets
   (for/list ([x free-vars] [i (in-naturals 1)])
     (cons x (Prim 'vector-ref (list (Var clos-vec-name) (Int i)))))
   new-body))

(define (convert-type t)
  (match t
    [`(,ts ... -> ,rt)
     `(Vector (Vector _ ,@(map convert-type ts) -> ,(convert-type rt)))]
    [`(Vector ,ts ...)
     `(Vector ,@(map convert-type ts))]
    [else t]))
