(define (expmod base exp m)
    (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n a)
    (= (expmod a n n) a))

(define (Carmichael n)
    (define (iter-carmic n a)
        (if (= n a) true
            (if (= (expmod a n n) a)
                (iter-carmic n (+ a 1))
                false)
        ))
    (iter-carmic n 1))
