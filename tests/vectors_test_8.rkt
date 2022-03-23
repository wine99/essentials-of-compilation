(let ([x (vector 0 1)])
  (let ([n (read)])
    (begin
      (while (> n 1)
        (begin
          (vector-set! x 1 (+ (vector-ref x 0)
                              (vector-ref x 1)))
          (vector-set! x 0 (- (vector-ref x 1)
                              (vector-ref x 0)))
          (set! n (- n 1))))
      (vector-ref x 1))))
