(-
 (let ([x (if (eq? (read) 0)
              0
              (read))])
   (if (eq? x 42)
       x
       (+ x
          (if (and (> x 40) (< x 42))
              1
              0))))
 1)