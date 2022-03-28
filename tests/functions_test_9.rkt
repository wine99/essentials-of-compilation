(define (f [a : (Vector Integer)] [b : Boolean]) : Boolean
  (if (eq? (vector-ref a 0) 42)
      b
      (begin (vector-set! a 0 42)
             (not b))))

(define (a) : Integer
  0)

(let ([a (vector (a))])
  (if (f a #f)
      (vector-ref a 0)
      0))
