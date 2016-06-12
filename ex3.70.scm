
(cd "/home/lancecopper/code/SICP/")
(load "/home/lancecopper/code/SICP/ex3.51.scm")

(define (weight p)
    (+ (car p) (cadr p)))


(define (pairs s1 s2 weight)
    (define (merge-weighted s1 s2)
        (if (and (not (null? s1)) (not (null? s2)))
            (if (> (weight (stream-car s1)) (weight (stream-car s2)))    
                (cons-stream (stream-car s2)
                             (merge-weighted s1 (stream-cdr s2)))
                (cons-stream (stream-car s1)
                            (merge-weighted (stream-cdr s1) s2)))
            (if (stream-null? s1) 
                s2
                s1)))
    (cons-stream
        (list (stream-car s1) (stream-car s2))
        (merge-weighted
            (stream-map (lambda (x) (list (stream-car s1) x))
                        (stream-cdr s2))
            (pairs (stream-cdr s1) (stream-cdr s2) weight))))

;;; a
(define s (pairs integers integers weight))

(test s 10)



;;; b

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


(define (weight1 p)
    (+ (* 2 (car p)) (* 3 (cadr p)) (* 5 (car p) (cadr p))))

(define (pred num)
    (define (divisible? x y)
        (if (zero? (remainder x y))
            #t
            #f))
    (and (not (divisible? num 2))
         (not (divisible? num 3))
         (not (divisible? num 5))))

(define i235 (stream-filter pred integers))
          
(define s (pairs i235 i235 weight1)))

(test s 10)




