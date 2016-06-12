
;;;a
(define (reduce-terms n d)
    (let* ((termsgcd gcd-terms n d)
           (o2 (order termsgcd))
           (o1 (max (first-term (order n)) (first-term (order n))))
           (const (expt (coeff termsgcd) (+ 1 (- o1 o2))))
           ;;; multiply both numerator and denominator by const(c^(1+o1-o2))
           (n1 (mul-term-by-all-terms n (make-term 0 const)))
           (d1 (mul-term-by-all-terms d (make-term 0 const)))
           ;;; divide bot numerator and denominatro by gcd
           (n2 (div-terms n1 (list termsgcd)))
           (d2 (div-terms d1 (list termsgcd)))
           ;;; nd-gcd is  the (integer) greatest common divisor of all 
           ;;; the coefficients of the numerator and the denominator
           (n-gcd (apply gcd (map cadr n2)))
           (d-gcd (apply gcd (map cadr d2)))
           (nd-gcd (gcd n-gcd d-gcd))
           (nn (div-terms n2 (make-term 0 nd-gcd)))
           (dd (div-terms d2 (make-term 0 nd-gcd))))
          (cons nn dd)))

(define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((terms (reduce-terms (term-list p1) (term-list p2)))
              (var (variable p1)))
             (cons (make-poly var (car terms)) 
                   (make-poly var (cdar terms))))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))

        
;;; b

(define (reduce-integers n d)
  (let ((g (gcd n d)))
    (list (/ n g) (/ d g))))

(put 'reduce '(scheme-number scheme-number) reduce-integers)
(put 'reduce '(polynomial polynomial) reduce-poly)

(define (reduce n d)
    (apply-generic 'reduce n d))


(define (make-rat n d)
    (let ((nd (reduce nd))))
         (cons (car nd) (cdar nd)))



;;; test
(define p1 (make-polynomial 'x '((1 1)(0 1))))
(define p2 (make-polynomial 'x '((3 1)(0 -1))))
(define p3 (make-polynomial 'x '((1 1))))
(define p4 (make-polynomial 'x '((2 1)(0 -1))))

(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))

(add rf1 rf2)



