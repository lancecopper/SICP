
;;;a

;;; answer
(define p1 (make-polynomial 'x '((2 1) (1 -2) (0 1))))
(define p2 (make-polynomial 'x '((2 11) (1 7))))
(define p1 (make-polynomial 'x '((1 13) (0 5))))
(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

(put 'greatest-common-divisor '(polynomial polynomial) 
         (lambda (a b) (tag (gcd-poly a b))))

(define (greatest-common-divisor a b) 
  (apply-generic 'greatest-common-divisor a b))

(greatest-common-divisor q1 q2)



;;; auxiliary
(define (div-terms L1 L2) 
  (if (empty-termlist? L1) 
    (list (the-empty-termlist) (the-empty-termlist)) 
    (let ((t1 (first-term L1)) 
          (t2 (first-term L2))) 
      (if (> (order t2) (order t1)) 
        (list (the-empty-termlist) L1) 
        (let ((new-c (div (coeff t1) (coeff t2))) 
          (new-o (- (order t1) (order t2))) 
          (new-t (make-term new-o new-c))) 
          (let ((rest-of-result 
                  (div-terms (add-terms L1 (negate (mul-terms (list new-t) L2))) 
                              L2))) 
            (list (adjoin-term new-t 
                               (car rest-of-result)) 
                  (cadr rest-of-result))))))))


(define (gcd-poly p1 p2) 
  (if (same-varaible? (variable p1) (variable p2)) 
        (make-poly (variable p1) 
                   (gcd-terms (term-list p1) 
                              (term-list p2)) 
        (error "not the same variable -- GCD-POLY" (list p1 p2)))))


(define (make-polynomial variable term-list)
  ((get 'make 'polynomial) variable term-list))

(define (make-term order coeff) (list order coeff))

(define (make-rat n d)
    (let ((g (greatest-common-divisor n d)))
      (cons (div n g) (div d g))))


;;; a
(define (pseudoremainder-terms p1 p2)
    (let ((t1 (first-term L1)) 
          (t2 (first-term L2)))
         (let ((pseudo (expt (coeff t2) (- (order t1) (order t2) -1))))
              (cadr (div-terms (mul-term-by-all-terms p1 (make-term 0 pseudo)) p2)))))
    
(define (gcd-terms a b) 
  (if (empty-termlist? b) 
    a 
    (gcd-terms b (pseudoremainder-terms a b))))

;;; b

(define (gcd-terms a b) 
  (if (empty-termlist? b) 
    (let* ((coeff-list (map cadr a)) 
            (gcd-coeff (apply gcd coeff-list))) 
       (div-terms a (make-term 0  gcd-coeff)))
    (gcd-terms b (pseudoremainder-terms a b))))


