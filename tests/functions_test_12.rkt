(define (f [a : Integer]
           [b : Integer]
           [c : Integer]
           [d : Integer]
           [e : Integer]
           [f : Integer]
           [g : Integer])
        : Integer
  (begin (set! f 42)
         f))

(f 0 0 0 0 0 0 0)
