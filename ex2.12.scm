(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
    (make-interval (- (lower-bound x) (upper-bound y))
                   (- (upper-bound x) (lower-bound y))
    ))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-internal x y)
    (if (< (* (lower-bound y) (upper-bound y)) 0)
        (error "Division erro (interval spans 0)" y)
        (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))

(define (lower-bound x) (min (car x) (cdr x)))

(define (upper-bound x) (max (car x) (cdr x)))


(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (old-mul-interval x y) 
   (let ((p1 (* (lower-bound x) (lower-bound y))) 
         (p2 (* (lower-bound x) (upper-bound y))) 
         (p3 (* (upper-bound x) (lower-bound y))) 
         (p4 (* (upper-bound x) (upper-bound y)))) 
     (make-interval (min p1 p2 p3 p4) 
                    (max p1 p2 p3 p4))))


(define (mul-interval x y)
    (cond ((and (< (lower-bound x) 0) 
                (< (upper-bound x) 0) 
                (< (lower-bound y) 0) 
                (< (upper-bound y) 0))
           (make-interval (* (upper-bound x) (upper-bound y)) 
                          (* (lower-bound x) (lower-bound y)))
           )
        ((and (< (lower-bound x) 0) 
                (not (< (upper-bound x) 0)) 
                (< (lower-bound y) 0) 
                (< (upper-bound y) 0))
           (make-interval (* (upper-bound x) (lower-bound y)) 
                          (* (lower-bound x) (lower-bound y)))
           )
        ((and (not (< (lower-bound x) 0)) 
                (not (< (upper-bound x) 0)) 
                (< (lower-bound y) 0) 
                (< (upper-bound y) 0))
           (make-interval (* (upper-bound x) (lower-bound y)) 
                          (* (lower-bound x) (upper-bound y)))
           )
        ((and (< (lower-bound x) 0) 
                (< (upper-bound x) 0) 
                (< (lower-bound y) 0) 
                (not (< (upper-bound y) 0)))
           (make-interval (* (lower-bound x) (upper-bound y)) 
                          (* (lower-bound x) (lower-bound y)))
           )
        ((and (< (lower-bound x) 0) 
                (not (< (upper-bound x) 0)) 
                (< (lower-bound y) 0) 
                (not (< (upper-bound y) 0)))
           (make-interval (min (* (upper-bound x) (lower-bound y))
                               (* (lower-bound x) (upper-bound y)))
                          (max (* (upper-bound x) (upper-bound y))
                               (* (lower-bound x) (lower-bound y))))
           )
        ((and (not (< (lower-bound x) 0)) 
                (not (< (upper-bound x) 0)) 
                (< (lower-bound y) 0) 
                (not (< (upper-bound y) 0)))
           (make-interval (* (upper-bound x) (lower-bound y)) 
                          (* (upper-bound x) (upper-bound y)))
           )
        ((and (< (lower-bound x) 0) 
                (< (upper-bound x) 0) 
                (not (< (lower-bound y) 0)) 
                (not (< (upper-bound y) 0)))
           (make-interval (* (lower-bound x) (upper-bound y)) 
                          (* (upper-bound x) (lower-bound y)))
           )
        ((and (< (lower-bound x) 0) 
                (not (< (upper-bound x) 0)) 
                (not (< (lower-bound y) 0)) 
                (not (< (upper-bound y) 0)))
           (make-interval (* (lower-bound x) (upper-bound y)) 
                          (* (upper-bound x) (upper-bound y)))
           )
        ((and (not (< (lower-bound x) 0)) 
                (not (< (upper-bound x) 0)) 
                (not (< (lower-bound y) 0)) 
                (not (< (upper-bound y) 0)))
           (make-interval (* (lower-bound x) (lower-bound y)) 
                          (* (upper-bound x) (upper-bound y)))
           )))

 (define (eql-interval? a b) 
   (and (= (upper-bound a) (upper-bound b)) 
        (= (lower-bound a) (lower-bound b)))) 
  
 ;; Fails if the new mult doesn't return the same answer as the old 
 ;; naive mult. 
 (define (ensure-mult-works aH aL bH bL) 
   (let ((a (make-interval aL aH)) 
         (b (make-interval bL bH))) 
   (if (eql-interval? (old-mul-interval a b) 
                      (mul-interval a b)) 
       true 
       (error "new mult returns different value!"  
              a  
              b  
              (old-mul-interval a b) 
              (mul-interval a b)))))


(ensure-mult-works  +10 +10   +10 -10)
(mul-interval (make-interval +10 +10) (make-interval +10 -10))
(old-mul-interval (make-interval +10 +10) (make-interval +10 -10))

(ensure-mult-works  +10 +10   +10 +10) 
 (ensure-mult-works  +10 +10   +00 +10) 
 (ensure-mult-works  +10 +10   +00 +00) 
 (ensure-mult-works  +10 +10   +10 -10) 
 (ensure-mult-works  +10 +10   -10 +00) 
 (ensure-mult-works  +10 +10   -10 -10) 
  
 (ensure-mult-works  +00 +10   +10 +10) 
 (ensure-mult-works  +00 +10   +00 +10) 
 (ensure-mult-works  +00 +10   +00 +00) 
 (ensure-mult-works  +00 +10   +10 -10) 
 (ensure-mult-works  +00 +10   -10 +00) 
 (ensure-mult-works  +00 +10   -10 -10) 
  
 (ensure-mult-works  +00 +00   +10 +10) 
 (ensure-mult-works  +00 +00   +00 +10) 
 (ensure-mult-works  +00 +00   +00 +00) 
 (ensure-mult-works  +00 +00   +10 -10) 
 (ensure-mult-works  +00 +00   -10 +00) 
 (ensure-mult-works  +00 +00   -10 -10) 
  
 (ensure-mult-works  +10 -10   +10 +10) 
 (ensure-mult-works  +10 -10   +00 +10) 
 (ensure-mult-works  +10 -10   +00 +00) 
 (ensure-mult-works  +10 -10   +10 -10) 
 (ensure-mult-works  +10 -10   -10 +00) 
 (ensure-mult-works  +10 -10   -10 -10) 
  
 (ensure-mult-works  -10 +00   +10 +10) 
 (ensure-mult-works  -10 +00   +00 +10) 
 (ensure-mult-works  -10 +00   +00 +00) 
 (ensure-mult-works  -10 +00   +10 -10) 
 (ensure-mult-works  -10 +00   -10 +00) 
 (ensure-mult-works  -10 +00   -10 -10) 
  
 (ensure-mult-works  -10 -10   +10 +10) 
 (ensure-mult-works  -10 -10   +00 +10) 
 (ensure-mult-works  -10 -10   +00 +00) 
 (ensure-mult-works  -10 -10   +10 -10) 
 (ensure-mult-works  -10 -10   -10 +00) 
 (ensure-mult-works  -10 -10   -10 -10) 
  

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))





(define (make-interval-center-percent c rt)
  (make-interval (- c (* c (/ rt 100))) (+ c (* c (/ rt 100)))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))

(define (percent-tolerance i)
    (let ((center (/ (+ (upper-bound i) (lower-bound i)) 2.0)) 
         (width (/ (- (upper-bound i) (lower-bound i)) 2.0))) 
     (* (/ width center) 100)))

 ;; A quick check: 
  
 (define i (make-interval-center-percent 10 50)) 
 (lower-bound i) 
 (upper-bound i) 
 (center i) 
 (percent-tolerance i) 
  
  
 ; The above returns 
 ;Value: i 
 ;Value: 5. 
 ;Value: 15. 
 ;Value: 10. 
 ;Value: 50. 