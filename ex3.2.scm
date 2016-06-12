(define (make-monitor f)
    (define times 0)
    (define (how-many-calls?)
        times)
    (define (reset-count)
        (set! times 0)
        times)
    (define (dispatch m)
        (cond ((eq? m 'how-many-calls?) (how-many-calls?))
              ((eq? m 'reset-count) (reset-count))
              (else (set! times (+ times 1))
                    (f m))))
    dispatch)


;;; test

(define s (make-monitor sqrt))

(s 100)
(s 'how-many-calls?)
(s 'reset-count)


