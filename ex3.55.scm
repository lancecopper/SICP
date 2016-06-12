
(define (partial-sums s)
    (cons-stream (stream-car s) (add-streams (partial-sums s) (stream-cdr s))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define intergers (integers-starting-from 1))

(define s (partial-sums intergers))
(stream-ref s 0) 
(stream-ref s 1)
(stream-ref s 2)
(stream-ref s 3)
