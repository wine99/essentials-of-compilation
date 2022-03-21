#lang racket
(require racket/set racket/stream)
(require racket/fixnum)
(require "interp-Lwhile.rkt")
(require "type-check-Lwhile.rkt")
(require "interp-Cwhile.rkt")
(require "type-check-Cwhile.rkt")
(require "interp.rkt")
(require "passes/shrink.rkt")
(require "passes/uniquify.rkt")
(require "passes/uncover-get.rkt")
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
  `(("shrink" ,shrink ,interp-Lwhile #;,type-check-Lwhile)
    ("uniquify" ,uniquify ,interp-Lwhile #;,type-check-Lwhile)
    ("uncover get!" ,uncover-get! ,interp-Lwhile #;,type-check-Lwhile)
    ("remove complex opera*" ,remove-complex-opera* ,interp-Lwhile #;,type-check-Lwhile)
    ("explicate control" ,explicate-control ,interp-Cwhile)
    ("select instruction" ,select-instructions ,interp-pseudo-x86-1)
    ("uncover live" ,uncover-live ,interp-pseudo-x86-1)
    ("build interference" ,build-interference ,interp-pseudo-x86-1)
    ("allocate registers" ,allocate-registers ,interp-x86-1)
    ("patch instructions" ,patch-instructions ,interp-x86-1)
    ("prelude and conclusion" ,prelude-and-conclusion ,interp-x86-1))
  )

