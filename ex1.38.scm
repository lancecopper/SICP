(define (cont-frac n d k)
	(define (iter k result)
		(if (= k 0) result
			(iter (- k 1) (/ (n k) (+ (d k) result)))
		))
	(iter k 0))

(define (euler i)
	(if (= (remainder (- i 2) 3) 0)
		(* 2 (+ (quotient (- i 2) 3) 1))
		1))

(cont-frac (lambda (i) 1.0)
           euler
           k)

