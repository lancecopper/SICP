
(define (rcl r l c dt)
    (define (rcl-vc-il vc0 il0)
        (define vc (integral (delay dvc) vc0 dt))
        (define il (integral (delay dil) il0 dt))
        (define dvc (scale-stream il (/ -1 C)))
        (define dil 
            (add-streams 
                (scale-stream vc (/ 1 l))
                (scale-stream il (/ (- r) l))))
        (cons vc il))
    rcl-vc-il)
 
;;; test
(define rcl1 (rcl 1 1 0.2 0.1))
(define s (rcl1 10 0))

(test (car s) 10)
(test (cdr s) 10)









