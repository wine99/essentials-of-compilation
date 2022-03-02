#lang racket
(require racket/set racket/stream)
(require racket/fixnum)
(require "interp-Lvar.rkt")
(require "interp-Cvar.rkt")
(require "interp.rkt")
(require "passes/uniquify.rkt")
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
  `(("uniquify" ,uniquify ,interp-Lvar)
    ("remove complex opera*" ,remove-complex-opera* ,interp-Lvar)
    ("explicate control" ,explicate-control ,interp-Cvar)
    ("select instruction" ,select-instructions ,interp-pseudo-x86-0)
    ("uncover live" ,uncover-live ,interp-pseudo-x86-0)
    ("build interference" ,build-interference ,interp-pseudo-x86-0)
    ("allocate registers" ,allocate-registers ,interp-x86-0)
    ("patch instructions" ,patch-instructions ,interp-x86-0)
    ("prelude and conclusion" ,prelude-and-conclusion ,interp-x86-0)
    ))

