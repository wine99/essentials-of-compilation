#! /usr/bin/env racket
#lang racket

(require "utilities.rkt")
(require "interp-Lvec.rkt")
(require "type-check-Lvec.rkt")
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


(interp-tests "vector" type-check-Lvec compiler-passes interp-Lvec "var_test" (tests-for "var"))
(interp-tests "vector" type-check-Lvec compiler-passes interp-Lvec "cond_test" (tests-for "cond"))
(interp-tests "vector" type-check-Lvec compiler-passes interp-Lvec "while_test" (tests-for "while"))
(interp-tests "vector" type-check-Lvec compiler-passes interp-Lvec "vectors_test" (tests-for "vectors"))


;; Uncomment the following when all the passes are complete to
;; test the final x86 code.
;(compiler-tests "vector" type-check-Lvec compiler-passes "var_test" (tests-for "var"))
;(compiler-tests "vector" type-check-Lvec compiler-passes "cond_test" (tests-for "cond"))
;(compiler-tests "vector" type-check-Lvec compiler-passes "while_test" (tests-for "while"))
;(compiler-tests "vector" type-check-Lvec compiler-passes "vectors_test" (tests-for "vectors"))
