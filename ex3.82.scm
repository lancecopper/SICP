(cd "/home/lancecopper/code/SICP/")
(load "/home/lancecopper/code/SICP/ex3.51.scm")
;;;

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))


(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (random-stream x1 x2)
    (cons-stream (random-in-range x1 x2)
                 (random-stream x1 x2)))

(define (predicate x y)
    (not (> (+ (square (- x 5)) (square (- y 7))) (square 1))))

(define (experiment-stream predicate x1 x2 y1 y2)
    (stream-map
        predicate
        (random-stream x1 x2)
        (random-stream y1 y2)))


(define estimate-pi-stream
    (scale-stream 
        (monte-carlo 
            (experiment-stream predicate 4.0 6.0 6.0 8.0) 0.0 0.0) 
        4))



(define estimate-pi-stream
    (scale-stream 
        (monte-carlo 
            (experiment-stream predicate 4.0 6.0 6.0 8.0) 0.0 0.0) 
        4))

(stream-ref estimate-pi-stream 10000)

(test estimate-pi-stream 10000)






