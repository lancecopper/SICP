

(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))

;;;a
100, 1000, 10000, 100000, 1000000

;;; b
1000000










