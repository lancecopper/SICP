
(define (delay exp)
    (lambda () exp))

(define (force delay-boj)
    (delay))


(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)


(define s (solve (lambda (y) y) 1 0.001))

(stream-ref s 10000)

(stream-ref (solve (lambda (y) y) 1 0.001) 100)


;;;
(define (integral delay-integrand initial-value dt)
    (cons-stream initial-value
                 (let ((integrand (force delay-integrand)))
                      (if (stream-null? integrand)
                          the-empty-stream
                          (integral (delay (stream-cdr integrand))
                                    (+ (* dt (stream-car integrand))
                                       initial-value)
                                    dt)))))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define s (solve (lambda (y) y) 1 0.001))

(test s 10)



