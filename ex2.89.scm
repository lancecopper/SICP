(define (first-term term-list) 
   (list (car term-list) (- (length term-list) 1)))

(define (adjoin-term term term-list) 
   (let ((exponent (order term)) 
         (len (length term-list))) 
     (define (iter-adjoin times terms) 
       (cond ((=zero? (coeff term)) 
              terms)) 
             ((= exponent times) 
              (cons (coeff term) terms)) 
             (else (iter-adjoin (+ times 1)  
                                (cons 0 terms)))) 
         (iter-adjoin len term-list))) 



(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  <procedures same-variable? and variable? from section 2.3.2>
  ;; representation of terms and term lists
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
