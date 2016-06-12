;;; answer
(define (install-sparse-package)
    (define (adjoin-term term term-list)
      (if (=zero? (coeff term))
          (cdr term-list)
          (cons term term-list)))
    (define (first-term term-list) term-list)

    (define (tag x) (cons 'dense x))
    (put 'adjoin-term 
         'sparse 
         (lambda (term term-list) 
                 (tag (adjoin-term term term-list))))
    (put 'first-term '(sparse) first-term)
    'done)


(define (install-dense-package)
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
  (define (first-term term-list) 
    (list (car term-list) (- (length term-list) 1))) 

  (define (tag x) (cons 'dense x)
  (put 'adjoin-term 
       'dense 
       (lambda (term term-list) 
               (tag (adjoin-term term term-list))))
  (put 'first-term '(dense) first-term)
  'done)



(define (rest-terms term-list) (cddr term-list))
(define (empty-termlist? term-list) (null? (cdr term-list)))
(define (make-term order coeff) (list order coeff))
(define (the-empty-termlist) (cons 'sparse '())
(define (order term) (car term))
(define (coeff term) (cadr term)
(define (adjoin-term term term-list)
  (let ((proc (get 'adjoin-term (type-tag term-list))))
       (if proc
           (apply proc (map contents args))
           (error
            "No adjoin-term for these type -- adjoin-term"
            term-list))))
(define (first-term term-list)
  (apply-generic 'first-term term-list))







;; representation of terms and term lists
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



(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (variable? x) (symbol? x))

  (define (zero?-poly poly) 
    (define (zero-terms? termlist) 
      (or (empty-termlist? termlist) 
          (and (=zero? (coeff (first-term termlist))) 
               (zero-terms? (rest-terms termlist))))) 
    (zero-terms? (term-list poly)))
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
     (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
     (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
     (lambda (var terms) (tag (make-poly var terms))))
  (put 'zero 'polynomial
     zero?-poly)
  'done)