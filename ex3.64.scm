
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
   guesses)

(define (stream-limit s tolerance)
    (if (< (abs (- (stream-car s) (stream-car (stream-cdr s)))) tolerance)
        (stream-car (stream-cdr s))
        (stream-limit (stream-cdr s) tolerance)))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;;; test
(define (average x y)
    (/ (+ x y) 2))

(sqrt 9 0.001)

