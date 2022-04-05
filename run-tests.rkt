#! /usr/bin/env racket
#lang racket

(require "utilities.rkt")
(require "interp-Lfun.rkt")
(require "type-check-Lfun.rkt")
(require "interp.rkt")
(require "compiler.rkt")


;(debug-level 1)
(AST-output-syntax 'concrete-syntax)

;; all the files in the tests/ directory with extension ".rkt".
(define all-tests
  (map (lambda (p) (car (string-split (path->string p) ".")))
       (filter (lambda (p)
                 (string=? (cadr (string-split (path->string p) ".")) "rkt"))
               (directory-list (build-path (current-directory) "tests")))))

(define (tests-for r)
  (map (lambda (p)
         (caddr (string-split p "_")))
       (filter
        (lambda (p)
          (string=? r (car (string-split p "_"))))
        all-tests)))


;(interp-tests "fun" type-check-Lfun compiler-passes interp-Lfun "var_test" (tests-for "var"))
;(interp-tests "fun" type-check-Lfun compiler-passes interp-Lfun "cond_test" (tests-for "cond"))
;(interp-tests "fun" type-check-Lfun compiler-passes interp-Lfun "while_test" (tests-for "while"))
;(interp-tests "fun" type-check-Lfun compiler-passes interp-Lfun "vectors_test" (tests-for "vectors"))
;(interp-tests "fun" type-check-Lfun compiler-passes interp-Lfun "functions_test" (tests-for "functions"))


;; Uncomment the following when all the passes are complete to
;; test the final x86 code.
(compiler-tests "fun" type-check-Lfun compiler-passes "var_test" (tests-for "var"))
(compiler-tests "fun" type-check-Lfun compiler-passes "cond_test" (tests-for "cond"))
(compiler-tests "fun" type-check-Lfun compiler-passes "while_test" (tests-for "while"))
(compiler-tests "fun" type-check-Lfun compiler-passes "vectors_test" (tests-for "vectors"))
(compiler-tests "fun" type-check-Lfun compiler-passes "functions_test" (tests-for "functions"))
