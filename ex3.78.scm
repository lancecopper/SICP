(define (integral delay-integrand initial-value dt)
    (cons-stream initial-value
                 (let ((integrand (force delay-integrand)))
                      (if (stream-null? integrand)
                          the-empty-stream
                          (integral (delay (stream-cdr integrand))
                                    (+ (* dt (stream-car integrand))
                                       initial-value)
                                    dt)))))


(define (solve-2nd a b dt y0 dy0)
  (define (f x y) (+ (* a x) (* b y)))
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

(define s (solve-2nd 1 1 0.001 1 0.1))

(test s 10)



