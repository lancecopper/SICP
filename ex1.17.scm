(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(define (double x) (* x 2))

(define (halve x) (/ x 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (mult a b)
	(if (=  b 1) a
		(if (even? b) (mult (double a) (halve b))
			(+ a (mult a (- b 1)))
		)))
