
(define (invert-unit-series s)
    (define stream
        (cons-stream 
            1 
            (scale-stream 
                (mul-series (stream-cdr s) stream) 
                -1)))
    stream)

(define (div-series s1 s2)
    (if (= (car s2) 0)
        (error "should begin with a nonzero constant term--" s2)
        (scale-stream
            (mul-series s1
                        (invert-unit-series 
                            (scale-stream s2 (/ 1 (stream-car s2)))))
            (/ 1 (stream-car s2)))))


;;; test
(define rs (div-series integers integers))

(test rs 10)


