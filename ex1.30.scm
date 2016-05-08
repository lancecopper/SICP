
(define (cube x) (* x x x)) 
  
(define (inc n) (+ n 1)) 

(define (pi-sum a b) 
  (define (pi-term x) 
          (/ 1.0 (* x (+ x 2)))) 
  (define (pi-next x) 
          (+ x 4)) 
  (itersum pi-term a pi-next b)) 


(define (Simpson f a b n)
	(define h (/ (- b a) n))
	(define (yk k) (f (+ a (* h k))))
	(define (simpson-term k) 
     (* (cond ((or (= k 0) (= k n)) 1) 
              ((odd? k) 4) 
              (else 2)) 
        (yk k))) 

	(define (next x) (+ x h))
	(define (term x) (* (f x) (multiplier)))
	(* (/ h 3) (sum simpson-term 0 inc n)))


(define (sum term a next b)
  (define (iter a result)
    (if  (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

