(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

def foo():
    pass

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (gcd m n)
   (cond ((< m n) (gcd n m)) 
         ((= n 0) m) 
         (else (gcd n (remainder m n)))))

(define (make-rat n d)
    (cond ((= d 0) (begin (newline) (display "Error: denom cannot be zero!")))
        ((= n 0) 0)
        ((and (> n 0) (> d 0))
            (let ((g (gcd n d)))
                (cons (/ n g) (/ d g))))
        ((and (< n 0) (> d 0))
            (let ((g (gcd (- n) d)))
                (cons (/ n g) (/ d g))))
        ((and (> n 0) (< d 0))
            (let ((g (gcd n (- d))))
                (cons (- (/ n g)) (- (/ d g)))))
        ((and (< n 0) (< d 0))
            (let ((g (gcd (- n) (- d))))
                (cons (- (/ n g)) (- (/ d g)))))
    ))
('1','110000','北京市');


