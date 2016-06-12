
(define (mul-streams s1 s2)
    (stream-map * s1 s2))


(define (div-streams s1 s2)
    (stream-map / s1 s2))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define intergers (integers-starting-from 1))

(define (integrate-series s)
    (div-streams s intergers))

(define ones (cons-stream 1 ones))

;;; a
(define s (integrate-series ones))

(define (test n)
    (if (< n 0)
        'done
        (begin (display (stream-ref s n))
               (newline)
               (test (- n 1)))))

(test s 10)


;;; b
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define s exp-series)

(test s 10)

(define sine-series (cons-stream 0 (integrate-series cosine-series)))
(define cosine-series (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

(define s sine-series)
(test s 10)


(define s cosine-series)
(test s 10)


