(define tolerance 0.00001)

(define (fixed-point f first-guess)
  	(define (close-enough? v1 v2)
    	(< (abs (- v1 v2)) tolerance))
  	(define (try guess)
		(newline)
		(display guess)
    	(let ((next (f guess)))
      	(if (close-enough? guess next)
          	next
          	(try next))))
  	(try first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 1.1)
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  	(define (close-enough? v1 v2)
    	(< (abs (- v1 v2)) tolerance))
  	(define (try guess)
		(newline)
		(display guess)
    	(let ((next (/ (+ (f guess) guess) 2)))
      	(if (close-enough? guess next)
          	next
          	(try next))))
  	(try first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 1.1)