(define (invert-unit-series s)
    (define stream
        (cons-stream 
            1 
            (scale-stream 
                (mul-series (stream-cdr s) stream) 
                -1)))
    stream)

;;; test
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define s (invert-unit-series integers))

(test s 100)

(define rs (mul-series s integers))

(test rs 10)


