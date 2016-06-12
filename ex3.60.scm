
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))


(define (add-streams s1 s2)
  (stream-map + s1 s2))


(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2)) 
               (add-streams 
                    (add-streams (scale-stream (stream-cdr s2) 
                                               (stream-car s1))
                                 (scale-stream (stream-cdr s1) 
                                               (stream-car s2)))         
                    (cons-stream 0 (mul-series (stream-cdr s1) 
                                               (stream-cdr s2))))))

(define one (add-streams (mul-series cosine-series cosine-series) 
            (mul-series sine-series sine-series)))

(test one 10)


