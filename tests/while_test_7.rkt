; Collatz conjecture

(let ([n 11])
  (begin
    (while (not (eq? n 1))
      (let ([t n])
        (begin
          (while (> t 0)
            (set! t (- t 2)))
          (if (< t 0)
              (set! n (+ n (+ n (+ n 1))))
              (let ([t1 0])
                (let ([t2 0])
                  (begin
                    (while (not (eq? t2 n))
                      (begin
                        (set! t1 (+ t1 1))
                        (set! t2 (+ t2 2))))
                    (set! n t1))))))))
    n))
