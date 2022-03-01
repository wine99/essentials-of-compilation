(+ (read)
   (let ([x (+ (let ([y (read)]) (+ y 1))
               (read))])
     x))