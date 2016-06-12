(define (make-vect x y)
    (cons x y))

(define (xcor-vect vector)
    (car vector))

(define (ycor-vect vector)
    (cdr vector))

(define (add-vect v1 v2)
    (make-vect (+ (xcor-vect v1) (xcor-vect v2)) 
               (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
    (make-vect (- (xcor-vect v1) (xcor-vect v2)) 
               (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect scale vector)
    (make-vect (* scale (xcor-vect vector)) 
               (* scale (ycor-vect vector))))




(define (make-segment start-vector end-vector)
    (cons start-vector end-vector))

(define (start-segment segment)
    (car segment))

(define (end-segment segment)
    (cdr segment))
















