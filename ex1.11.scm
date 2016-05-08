(define (f n)
	(if (< n 3) n
		(+ (f (- n 1)) 
		   (* 2 (f (- n 2)))
		   (* 3 (f (- n 3)))
		)))

(define (f-iter a b c count)
	(if (= count 0) c
		(f-iter b c (+ (* 3 a) (* 2 b) c) (- count 1))
	))


(define (f1 n)
	(if (< n 3) n
		(f-iter 0 1 2 (- n 2))
	))
