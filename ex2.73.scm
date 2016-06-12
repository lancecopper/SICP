(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (arg1 exp) var)
                   (deriv (arg2 exp) var)))
        ((product? exp)
         (make-sum
           (make-product (arg1 exp)
                         (deriv (arg2 exp) var))
           (make-product (deriv (arg1 exp) var)
                         (arg2 exp))))
        <more rules can be added here>
        (else (error "unknown expression type -- DERIV" exp))))


(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))


;;; b

(define (install-deriv-package)
    ;; internal procedures
    (define (arg1 s) (car s))
    (define (arg2 s) (cadr s))
    (define (make-sum a1 a2)
        (cond ((=number? a1 0) a2)
            ((=number? a2 0) a1)
            ((and (number? a1) (number? a2)) (+ a1 a2))
            (else (list '+ a1 a2))))
    (define (make-product m1 m2)
        (cond ((or (=number? m1 0) (=number? m2 0)) 0)
            ((=number? m1 1) m2)
            ((=number? m2 1) m1)
            ((and (number? m1) (number? m2)) (* m1 m2))
            (else (list '* m1 m2))))
    (define (sum-deriv exp var)
        (make-sum (deriv (arg1 exp) var)
                  (deriv (arg2 exp) var)))
    (define (product-deriv exp var)
        (make-sum
            (make-product (arg1 exp)
                          (deriv (arg2 exp) var))
            (make-product (deriv (arg1 exp) var)
                          (arg2 exp))))
    (put 'deriv '(+) sum-deriv)
    (put 'deriv '(*) product-deriv))

;;; c
;; answer



(define (install-deriv-package)
    ;; internal procedures
    (define (arg1 s) (car s))
    (define (arg2 s) (cadr s))
    (define (make-sum a1 a2)
        (cond ((=number? a1 0) a2)
            ((=number? a2 0) a1)
            ((and (number? a1) (number? a2)) (+ a1 a2))
            (else (list '+ a1 a2))))
    (define (make-product m1 m2)
        (cond ((or (=number? m1 0) (=number? m2 0)) 0)
            ((=number? m1 1) m2)
            ((=number? m2 1) m1)
            ((and (number? m1) (number? m2)) (* m1 m2))
            (else (list '* m1 m2))))
    (define (make-exponentiation x y)
        (cond ((= y 0) 1)
              ((= y 1) x)
              ((= x 1) 1)
              (else  (list '** x y))))
    (define (sum-deriv exp var)
        (make-sum (deriv (arg1 exp) var)
                  (deriv (arg2 exp) var)))
    (define (product-deriv exp var)
        (make-sum
            (make-product (arg1 exp)
                          (deriv (arg2 exp) var))
            (make-product (deriv (arg1 exp) var)
                          (arg2 exp))))
    (define (exponentiation-deriv exp var)
        (make-product
            (make-product (exponent exp)
                          (make-exponentiation (base exp) (- (exponent exp) 1)))
            (deriv (base exp) var)))
    (put 'deriv '(+) sum-deriv)
    (put 'deriv '(*) product-deriv)
    (put 'deriv '(**) exponentiation-deriv))


;;; d

(define (install-deriv-package)
    ;; internal procedures
    (define (arg1 s) (car s))
    (define (arg2 s) (cadr s))
    (define (make-sum a1 a2)
        (cond ((=number? a1 0) a2)
            ((=number? a2 0) a1)
            ((and (number? a1) (number? a2)) (+ a1 a2))
            (else (list '+ a1 a2))))
    (define (make-product m1 m2)
        (cond ((or (=number? m1 0) (=number? m2 0)) 0)
            ((=number? m1 1) m2)
            ((=number? m2 1) m1)
            ((and (number? m1) (number? m2)) (* m1 m2))
            (else (list '* m1 m2))))
    (define (make-exponentiation x y)
        (cond ((= y 0) 1)
              ((= y 1) x)
              (else  (list '** x y))))
    (define (sum-deriv exp var)
        (make-sum (deriv (arg1 exp) var)
                  (deriv (arg2 exp) var)))
    (define (product-deriv exp var)
        (make-sum
            (make-product (arg1 exp)
                          (deriv (arg2 exp) var))
            (make-product (deriv (arg1 exp) var)
                          (arg2 exp))))
    (define (exponentiation-deriv exp var)
        (make-product
            (make-product (exponent exp)
                          (make-exponentiation (base exp) (- (exponent exp) 1)))
            (deriv (base exp) var)))
    (put '(+) 'deriv sum-deriv)
    (put '(*) 'deriv product-deriv)
    (put '(**) 'deriv exponentiation-deriv))



(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get (operator exp) 'deriv) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))


