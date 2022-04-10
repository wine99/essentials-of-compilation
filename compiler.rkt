#lang racket
(require racket/set racket/stream)
(require racket/fixnum)
(require "interp-Llambda.rkt")
(require "type-check-Llambda.rkt")
(require "interp-Llambda-prime.rkt")
(require "interp-Clambda.rkt")
(require "type-check-Clambda.rkt")
(require "interp.rkt")
(require "passes/shrink.rkt")
(require "passes/uniquify.rkt")
(require "passes/uncover-get.rkt")
(require "passes/convert-assignments.rkt")
(require "passes/reveal-functions.rkt")
(require "passes/convert-to-closures.rkt")
(require "passes/limit-functions.rkt")
(require "passes/expose-allocation.rkt")
(require "passes/remove-complex-opera.rkt")
(require "passes/explicate-control.rkt")
(require "passes/select-instructions.rkt")
(require "passes/uncover-live.rkt")
(require "passes/build-interference.rkt")
(require "passes/allocate-registers.rkt")
(require "passes/patch-instructions.rkt")
(require "passes/prelude-and-conclusion.rkt")
(provide (all-defined-out))


;; Define the compiler passes to be used by interp-tests and the grader
;; Note that your compiler file (the file that defines the passes)
;; must be named "compiler.rkt"
(define compiler-passes
  `(("shrink" ,shrink ,interp-Llambda)
    ("uniquify" ,uniquify ,interp-Llambda)
    ("uncover get!" ,uncover-get! ,interp-Llambda)
    ("convert assignments" ,convert-assignments ,interp-Llambda)
    ("reveal functions" ,reveal-functions ,interp-Llambda-prime
                        ,type-check-Llambda)
    ("convert to closures" ,convert-to-closures ,interp-Llambda-prime)
    ("limit functions" ,limit-functions ,interp-Llambda-prime
                       ,type-check-Llambda)
    ("expose allocation" ,expose-allocation ,interp-Llambda-prime)
    ("remove complex opera*" ,remove-complex-opera* ,interp-Llambda-prime)
    ("explicate control" ,explicate-control ,interp-Clambda
                         ,type-check-Clambda)
    ("select instruction" ,select-instructions ,interp-pseudo-x86-3)
    ("uncover live" ,uncover-live ,interp-pseudo-x86-3)
    ("build interference" ,build-interference ,interp-pseudo-x86-3)
    ("allocate registers" ,allocate-registers ,interp-x86-3)
    ("patch instructions" ,patch-instructions ,interp-x86-3)
    ("prelude and conclusion" ,prelude-and-conclusion #f)
    )
  )

