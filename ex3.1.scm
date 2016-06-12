(define (make-accumulator )
    (lambda (num)
        (begin (set! x (+ num x)))
                x))


(define A (make-accumulator 5))
(A 10)




