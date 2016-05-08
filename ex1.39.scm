(define (tan-cf x k)
	(define (f i)
		(if (= i 1) x (- (* x x))))
	(define (g i)
		(- (* 2 i) 1))
	(cont-frac f g k))

(define (cont-frac n d k)
	(define (iter k result)
		(if (= k 0) result
			(iter (- k 1) (/ (n k) (+ (d k) result)))
		))
	(iter k 0))


