

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))


(define s (expand 1 7 10))
(define s (expand 3 8 10))

;;; test

(stream-ref s 0) 
(stream-ref s 1)
(stream-ref s 2)
(stream-ref s 3)



(define (test s n)
    (if (< n 0)
        'done
        (begin (display (stream-ref s n))
               (newline)
               (test s (- n 1)))))

(test s 10)

