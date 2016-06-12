(define f
    (let ((foo 0))
        (define (set-foo x)
            (cond ((and (= foo 0) (= x 0)) 0)
                  ((and (= foo 0) (not (= x 0))) (begin (set! foo x) 0))
                  ((not (= foo 0)) foo)))
        set-foo))

(+ (f 0) (f 1))

(+ (f 1) (f 0))







