(define dx 0.00001)


(define (repeated f n)
    (if (= n 1)
        (lambda (x) (f x))
        (compose f (repeated f (- n 1)))

(define (smooth f)
    (lambda (x) (\ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-fold-smooth f n) 
    ((repeated smooth n) f))
