(let ([n -1])
  (begin
    (if (< n 0)
        (set! n (- n))
        (set! n n))
    n))
