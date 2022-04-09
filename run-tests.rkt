#! /usr/bin/env racket
#lang racket

(require "utilities.rkt")
(require "interp-Llambda.rkt")
(require "type-check-Llambda.rkt")
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


(interp-tests "lambda" type-check-Llambda compiler-passes interp-Llambda "var_test" (tests-for "var"))
(interp-tests "lambda" type-check-Llambda compiler-passes interp-Llambda "cond_test" (tests-for "cond"))
(interp-tests "lambda" type-check-Llambda compiler-passes interp-Llambda "while_test" (tests-for "while"))
(interp-tests "lambda" type-check-Llambda compiler-passes interp-Llambda "vectors_test" (tests-for "vectors"))
(interp-tests "lambda" type-check-Llambda compiler-passes interp-Llambda "functions_test" (tests-for "functions"))
(interp-tests "lambda" type-check-Llambda compiler-passes interp-Llambda "lambda_test" (tests-for "lambda"))


;; Uncomment the following when all the passes are complete to
;; test the final x86 code.
;(compiler-tests "lambda" type-check-Llambda compiler-passes "var_test" (tests-for "var"))
;(compiler-tests "lambda" type-check-Llambda compiler-passes "cond_test" (tests-for "cond"))
;(compiler-tests "lambda" type-check-Llambda compiler-passes "while_test" (tests-for "while"))
;(compiler-tests "lambda" type-check-Llambda compiler-passes "vectors_test" (tests-for "vectors"))
;(compiler-tests "lambda" type-check-Llambda compiler-passes "functions_test" (tests-for "functions"))
;(compiler-tests "lambda" type-check-Llambda compiler-passes "lambda_test" (tests-for "lambda"))
