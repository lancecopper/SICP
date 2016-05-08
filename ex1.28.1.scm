(define (expmod base exp m)
    (cond ((= exp 0) 1)
        ((even? exp)
            (if (nontrivial? (expmod base (/ exp 2) m) m) 0
                (remainder (square (expmod base (/ exp 2) m))
                        m)))
        (else
            (remainder (* base (expmod base (- exp 1) m))
                        m))))


(define (nontrivial? x n)
    (if (and (not (= x 1)) (not (= x (- n 1))) (= (remainder (square x) n) 1))
        true
        false))


(define (Miller-Rabin-test n)
    (define (try-it a)
        (= (expmod a (- n 1) n) 1))
    (try-it (+ 1 (random (- n 1)))))



(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((Miller-Rabin-test n) (fast-prime? n (- times 1)))
        (else false)))


(define (square x) (* x x)) 


  
(define (divides? a b) 
    (= (remainder b a) 0)) 
 
  
(define (timed-prime-test n) 
    (start-prime-test n (runtime))) 
  
(define (start-prime-test n start-time) 
    (if (fast-prime? n 10) 
        (report-prime n (- (runtime) start-time)))) 
  
(define (report-prime n elapsed-time) 
    (newline) 
    (display n) 
    (display " *** ") 
    (display elapsed-time)) 


(define (search-for-primes first last)
    (define (search-iter cur last) 
        (if (<= cur last) (timed-prime-test cur)) 
        (if (<= cur last) (search-iter (+ cur 2) last))) 
    (search-iter (if (even? first) (+ first 1) first) 
                (if (even? last) (- last 1) last)))

(define (foo x y)
    (search-for-primes x y))
