(define (f) : Integer
  (begin (set! f g)
         (f)))

(define (g) : Integer 42)

(g)
