
(define (weight3 p)
    (+ (expt (car p) 2) (expt (cadr p) 2)))

(define pairs-i2-j2 (pairs integers integers weight3)))

(define (pairs2weight pairs weight)
    (cons-stream
        (weight (stream-car pairs))
        (pairs2weight (stream-cdr pairs) weight)))

(define (find-repetive s)
    (define (find-1st-same s1 s2)
        (if (= (stream-car s1) (stream-car s2))
            s1
            (find-1st-same (stream-cdr s1) (stream-cdr s2))))
    (let ((1st-same (find-1st-same s (stream-cdr s))))
         (cons-stream (stream-car 1st-same)
                      (find-repetive (stream-cdr 1st-same)))))

(define (skip-repetive s)
    (define (find-1st-diff s1 s2)
        (if (not (= (stream-car s1) (stream-car s2)))
            s1
            (find-1st-diff (stream-cdr s1) (stream-cdr s2))))
    (let ((1st-diff (find-1st-diff s (stream-cdr s))))
         (cons-stream (stream-car 1st-diff)
                      (skip-repetive (stream-cdr 1st-diff)))))

(define s 
    (skip-repetive 
        (find-repetive 
            (find-repetive 
                (pairs2weight pairs-i2-j2 weight3)))))


(test s 10)







