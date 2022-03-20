#! /usr/bin/env racket
#lang racket

(require "utilities.rkt")
(require "interp-Lwhile.rkt")
(require "type-check-Lwhile.rkt")
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

(interp-tests "while" type-check-Lwhile compiler-passes interp-Lwhile "var_test" (tests-for "var"))
(interp-tests "while" type-check-Lwhile compiler-passes interp-Lwhile "cond_test" (tests-for "cond"))
(interp-tests "while" type-check-Lwhile compiler-passes interp-Lwhile "while_test" (tests-for "while"))

;; Uncomment the following when all the passes are complete to
;; test the final x86 code.
;(compiler-tests "while" type-check-Lwhile compiler-passes "var_test" (tests-for "var"))
;(compiler-tests "while" type-check-Lwhile compiler-passes "cond_test" (tests-for "cond"))
;(compiler-tests "while" type-check-Lwhile compiler-passes "while_test" (tests-for "while"))
