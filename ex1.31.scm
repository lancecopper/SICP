(define (product term a next b) 
     (if (> a b) 1 
        (* (term a) (product term (next a) next b)))) 

(define (inc n) (+ n 1)) 

(define (identity x) x)

(define (factorial x)
    (product identity 1 inc x))

(define (pi n)
    (define (pi-term x)
        (define denominator (+ (* 2.0 x) 1))
        (/ (* (- denominator 1 ) (+ denominator 1)) (square denominator)))
    (* 4 (product pi-term 1 inc n)))


(define (product term a next b)
    (define (iter a result)
        (if (> a b) result
            (* (term a) (iter (next a) result))))
    (iter a 1))