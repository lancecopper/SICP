(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (cesaro-test x1 x2 y1 y2)
    (predicate
        (random-in-range x1 x2)
        (random-in-range y1 y2)))

(define (predicate x y)
    (not (> (+ (square (- x 5)) (square (- y 7))) (square 1))))

(define (monte-carlo-integration predicate x1 x2 y1 y2 trials)
    (let ((experiment (lambda () (cesaro-test predicate x1 x2 y1 y2))))
        (monte-carlo trials experiment)))

(define (estimate-integral predicate x1 x2 y1 y2 trials)
    (* (monte-carlo-integration predicate x1 x2 y1 y2 trials)
        (- x2 x1) (- y2 y1)))
    
(define (estimate-pi)
    (* 4 (monte-carlo-integration predicate 4.0 6.0 6.0 8.0 10000000000.0)))

;;; test

(estimate-pi)








