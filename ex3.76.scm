
(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))



(define (smooth input-stream)
    (define (average x y)
        (/ (+ x y) 2))
    (stream-map 
        average 
        input-stream 
        (cons-stream 0 input-stream)))


(define (make-smooth-zero-crossings input-stream last-value)
    (define (smooth input-stream)
        (define (average x y)
            (/ (+ x y) 2))
        (stream-map 
            average 
            input-stream 
            (cons-stream last-value input-stream)))
    (make-zero-crossings (smooth input-stream) last-value))





