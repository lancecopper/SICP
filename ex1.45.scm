(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y) (/ (+ x y) 2))


(define (average-damp f)
  (lambda (x) (average x (f x))))


(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))


(define (compose f g)
    (lambda (x) (f (g x))))

(define (repeated f n)
    (if (= n 1)
        (lambda (x) (f x))
        (compose f (repeated f (- n 1)))))


(define (fhrt x)
  (fixed-point-of-transform (lambda (y) (/ x (expt y 3)))
                            (repeated average-damp 1)
                            1.0))

(define (pow b p) 
    (define (even? x) 
        (= (remainder x 2) 0)) 
    
    (define (sqr x) 
        (* x x)) 
    
    (define (iter res a n) 
        (if (= n 0) 
            res 
            (if (even? n) 
                (iter res (sqr a) (/ n 2)) 
                (iter (* res a) a (- n 1))))) 
    (iter 1 b p))

(define (nth-root n x)
    (if (< n 2) 0
        (fixed-point-of-transform (lambda (y) (/ x (pow y (- n 1))))
            (repeated average-damp (/ n 2))
            1.0)))



