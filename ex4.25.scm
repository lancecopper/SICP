(define (try a b)
  (if (= a 0) 1 b))


(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(unless (= b 0)
        (/ a b)
        (begin (display "exception: returning 0")
               0))


;;; question

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))


(define (foo x y)
    (if (= y 0)
        x
        (/ x y)))


