(define (filtered-accumulate filter combier null-value term a next b)
    (if (> a b) null-value
        (if (filter a) 
            (combier (term a)
                (filtered-accumulate filter combier null-value term (next a) next b))
            (filtered-accumulate filter combier null-value term (next a) next b))))


(define (filtered-accumulate filter combier null-value term a next b)
    (define (iter a result)
        (if (> a b) result
            (if (filter a)
                (iter (next a) (combier (term a) result))
                (iter (next a) result))))
    (iter a null-value))



(define (square-prime-sum a b)
    (filtered-accumulate prime? + 0 square a inc b))

(define (foo a b)
    (square-prime-sum a b))

(define (inc x) (+ x 1))

(define (next x)
    (if (=  x 2) 3
        (+ x 2)))

(define (find-divisor n test-divisor) 
    (cond ((> (square test-divisor) n) n) 
         ((divides? test-divisor n) test-divisor) 
         (else (find-divisor n (next test-divisor)))))

(define (square x) (* x x)) 
  
(define (smallest-divisor n) 
    (find-divisor n 2))
  
(define (divides? a b) 
    (= (remainder b a) 0)) 
  
(define (prime? n) 
    (if (< n 2) false
        (= n (smallest-divisor n)))) 

(define (gcd m n) 
   (cond ((< m n) (gcd n m)) 
         ((= n 0) m) 
         (else (gcd n (remainder m n)))))

(define (relative-prime? m n)
    (= (gcd m n) 1))

(define (product-of-relative-primes n) 
   (define (filter x) 
     (relative-prime? x n)) 
 (filtered-accumulate filter * 1 identity 1 inc n)) 



