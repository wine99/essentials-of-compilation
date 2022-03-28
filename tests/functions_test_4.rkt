(define (m [a : Integer]
           [b : Boolean]
           [c : (Vector Integer)]
           [d : Integer]
           [e : Boolean]
           [f : (Vector Integer)]
           [g : Integer]
           [h : Boolean]
           [i : (Vector Integer)])
        : Integer
  (begin (vector-set! c 0 42)
         (vector-ref i 0)))

(let ([v (vector 0)])
  (m 0 #t v
     0 #t v
     0 #t v))
