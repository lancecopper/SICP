

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)


(define (rc r c dt)
    (define (rc-v i v0)
        (add-streams
            (integral (scale-stream i (/ 1 c)) v0 dt)
            (scale-stream i r)))
    rc-v)


;;; test
(define RC1 (RC 5 1 0.5))

(define s (rc1 integers 1))
(test s 10)
