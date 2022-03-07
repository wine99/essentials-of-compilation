(let ([x (if (if (eq? 0 0) (eq? 1 1) (eq? 2 2))
             42
             43)])
  (- x 0))