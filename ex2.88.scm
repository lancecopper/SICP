(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))


(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1)
                              (rest-terms L2)))))))))



(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))
(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))

(put 'sub '(polynomial polynomial) 
     (lambda (x y) (tag (add-poly x (negate y))))) 

(define (install-neg)
    (define (neg-sche x)
        (make-scheme number (- x))
    (define (neg-rat x)
        (make-rational (- (numer x)) (denom x)))
    (define (neg-com x)
        (make-from-real-imag (- (real-part x))
             (- (imag-part x))))
    (define (neg-terms x)
        (if (empty-termlist? termlist) 
         the-empty-termlist 
         (let ((t (first-term termlist))) 
           (adjoin-term (make-term (order t) (neg (coeff t))) 
                        (neg-terms (rest-terms termlist)))))) 
    (put 'neg '(scheme-number) neg-sche)
    (put 'neg '(rational) neg-rat)
    (put 'neg '(complex) neg-com)
    (put 'neg 'polynomial 
          (lambda (poly) (make-polynomial (variable poly) 
                                          (neg-terms (term-list poly))))) 
    'done)

(define (neg x)
  (apply-generic 'neg x))

(put 'neg 'polynomial 
          (lambda (poly) (make-polynomial (variable poly) 
                                          (neg-terms (term-list poly))))) 









