#lang racket/gui
(require framework)
(require rackunit)
(require "utilities.rkt")
(require "type-check-Llambda.rkt")
(require "compiler.rkt")


(define f (new frame% [label "A Simple Compiler"]
                      [width 1560]
                      [height 720]))

(define compile-btn
  (new button% [label "compile"] [parent f] [callback (lambda (e b) (compile-cb))]))
(define main-pane (new horizontal-panel%
                       [parent f]
                       [style '(hscroll)]))


(define c (new editor-canvas%
               [parent main-pane]
               [min-width 400]))
(define program-text (new racket:text%))
(define mb (new menu-bar% [parent f]))
(define m-edit (new menu% [label "Edit"] [parent mb]))
(append-editor-operation-menu-items m-edit #f)
(send program-text set-max-undo-history 100)
(send c set-editor program-text)


(define passes-texts
  (for/list ([pass compiler-passes])
    (define pan (new vertical-panel% [parent main-pane]))
    (new message% [parent pan] [label (car pass)])
    (define c (new editor-canvas%
                   [parent pan]
                   [min-width 400]))
    (define t (new racket:text%))
    (send t lock #t)
    (send c set-editor t)
    t))




(define (compile-file typechecker passes)
  (lambda (program)
    (define file-base "gui-test")
    (define out-file-name "gui-test.s")
    (call-with-output-file
        out-file-name
      #:exists 'replace
      (lambda (out-file)
        (define sexp program)
        (define tsexp (test-typecheck typechecker sexp))
        (if tsexp
            (let ([x86 (let loop ([passes passes] [p tsexp] [pass-idx 0])
                         (cond [(null? passes) p]
                               [else
                                (define pass-info (car passes))
                                (define name      (list-ref pass-info 0))
                                (define pass      (list-ref pass-info 1))
                                (define type-checker
                                  (cond [(>= (length pass-info) 4)
                                         (list-ref pass-info 3)]
                                        [else #f]))
                                (define new-p^
                                  ((check-exception name file-base #f)
                                   (thunk (pass p))))
                                (define new-p (cond [type-checker
                                                     (type-checker new-p^)]
                                                    [else new-p^]))
                                (define t (list-ref passes-texts pass-idx))
                                (send t lock #f)
                                (send t erase)
                                (send t
                                      insert
                                      (let ([s (open-output-bytes)])
                                        (print new-p s)
                                        (get-output-string s)))
                                (send t lock #t)
                                (loop (cdr passes) new-p (add1 pass-idx))
                                ]))])
              (define x86-str (print-x86 x86))
              (write-string x86-str out-file)
              (newline out-file)
              (flush-output out-file)
              #t)
            #f)
        ))))


(define (test-typecheck tcer exp)
  (define (handler e)
    (for ([t (in-list passes-texts)])
      (send t lock #f)
      (send t erase)
      (send t lock #t))
    (let ([t (car passes-texts)])
      (send t lock #f)
      (send t insert (exn-message e))
      (send t lock #t))
    #f)
  (if (eq? tcer #f)
      exp
      (with-handlers ([exn:fail? handler])
        (tcer exp))))

(define ((check-exception name test-name error-expected) fn)
  (with-handlers
    ([exn:fail?
      (lambda (exn)
        (cond [error-expected 'expected-error]
              [else
               (displayln (format "encountered exception while testing '~a`, case ~a" name test-name))
               (raise exn)]))])
    (let ([res (fn)])
      (when (and (not (string? res)) (not (pair? res)) (not (eq? res #f)))
        (check-false error-expected (format "in check-exception, expected exception, not ~a" res)))
      res)))


(debug-level 1)
(AST-output-syntax 'concrete-syntax)
(gui-mode #t)


(define compiler (compile-file type-check-Llambda compiler-passes))

(define (compile-cb)
  (define-values (input-port output-port) (make-pipe))
  (send program-text save-port output-port 'text)
  (close-output-port output-port)
  (define p (for/list ([e (in-port read input-port)]) e))
  (close-input-port input-port)
  (compiler (parse-program `(program () ,@p))))


(send f show #t)
