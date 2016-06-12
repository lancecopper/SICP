(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
            (make-product
                (make-product (exponent exp)
                              (make-exponentiation (base exp) (- (exponent exp) 1)))
                (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

;;; answer
(define (exponentiation? x)
    (and (pair? x) (eq? (car x) '**)))
(define (base x)
    (cadr x))
(define (exponent x)
    (caddr x))
(define (make-exponentiation x y)
    (cond ((= y 0) 1)
          ((= y 1) x)
          (else  (list '** x y))))

;;; test

(deriv '(** x 1) 'x)



;;; The variables are symbols. They are identified by the primitive predicate symbol?:
(define (variable? x) (symbol? x))

;;; Two variables are the same if the symbols representing them are eq?:
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;;; Sums and products are constructed as lists:

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))


(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))



;;; A sum is a list whose first element is the symbol +:
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

;;; The addend is the second item of the sum list:
(define (addend s) (cadr s))

;;; The augend is the third item of the sum list:
(define (augend s) (caddr s))

;;; A product is a list whose first element is the symbol *:
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

;;; The multiplier is the second item of the product list:
(define (multiplier p) (cadr p))

;;; The multiplicand is the third item of the product list:
(define (multiplicand p) (caddr p))







