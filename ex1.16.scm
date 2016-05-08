(define (fast-expt b n)
    (define (iter b n a)
        (if (= n 0) a
            (if (even? n) (iter (square b) (/ n 2) a)
                (iter b (- n 1) (* a b))
            )))
    (iter b n 1))




(define (even? n)
  (= (remainder n 2) 0))
