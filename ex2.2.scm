(define (make-segment x y)
    (cons x y))

(define (start-segment x)
    (car x))

(define (end-segment x)
    (cdr x))


(define (make-point x y)
    (cons x y))

(define (x-point x)
    (car x))

(define (y-point x)
    (cdr x))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


(define (midpoint-segment segment)
    (define (average a b) (/ (+ a b) 2.0))
    (let ((a (start-segment segment))
          (b (end-segment segment)))
        (make-point (average (x-point a)
                             (x-point b))
                    (average (y-point a) (y-point b)))))
    


(midpoint-segment (make-segment (make-point 0. 0.) (make-point 4. 4.)))








