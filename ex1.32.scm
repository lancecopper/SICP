(define (accumulate combier null-value term a next b)
	(if (> a b) null-value
		(combier (term a) (accumulate combier null-value term (next a) next b))))

(define (accumulate combier null-value term a next b)
	(define (iter a result)
		(if (> a b) result
			(iter (next a) (combier (term a) result))))
	(iter a null-value))


(define (product term a next b)
    (define (iter a result)
        (if (> a b) result
            (* (term a) (iter (next a) result))))
    (iter a 1))

(define (inc n) (+ n 1)) 

(define (identity x) x)

(define (factorial x)
    (product identity 1 inc x))

(define (pi n)
    (define (pi-term x)
        (define denominator (+ (* 2.0 x) 1))
        (/ (* (- denominator 1 ) (+ denominator 1)) (square denominator)))
    (* 4 (accumulate * 1 pi-term 1 inc n)))
