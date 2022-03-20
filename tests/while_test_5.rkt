(let ([a 0])
  (let ([b 1])
    (let ([n (read)])
      (begin
        (while (> n 1)
          (begin
            (set! b (+ a b))
            (set! a (- b a))
            (set! n (- n 1))))
        b))))
