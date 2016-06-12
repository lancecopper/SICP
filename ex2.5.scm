(define (cons-2-3 a b)
	(* (expt 2 a) (expt 3 b)))

(define (car-2-3 x)
	(define (iter x result)
		(if (=  (remainder x 2) 0)
			(iter (/ x 2) (+ result 1))
			result))
	(iter x 0))

(define (cdr-2-3 x)
	(define (iter x result)
		(if (=  (remainder x 3) 0)
			(iter (/ x 3) (+ result 1))
			result))
	(iter x 0))

