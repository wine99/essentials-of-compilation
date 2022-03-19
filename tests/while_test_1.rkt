(let ([x 2])
  (+ x (begin (set! x 40) x)))