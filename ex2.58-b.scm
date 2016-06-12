;;;; b

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
        (else
         (error "unknown expression type -- DERIV" exp))))


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
        (else (list a1 '+  a2))))


(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))



;;; A sum is a list whose first element is the symbol +:
(define (sum? expr) 
    (eq? '+ (smallest-op expr))) 

;;; The addend is the second item of the sum list:
(define (addend expr) 
   (let ((a (prefix '+ expr))) 
     (if (singleton? a) 
         (car a) 
         a)))

;;; The augend is the third item of the sum list:
(define (augend expr) 
   (let ((a (cdr (memq '+ expr)))) 
     (if (singleton? a) 
         (car a) 
         a)))

;;; A product is a list whose first element is the symbol *:
(define (product? expr) 
    (eq? '* (smallest-op expr))) 

;;; The multiplier is the second item of the product list:
(define (multiplier expr) 
   (let ((m (prefix '* expr))) 
     (if (singleton? m) 
         (car m) 
         m)))

;;; The multiplicand is the third item of the product list:
(define (multiplicand expr) 
   (let ((m (cdr (memq '* expr)))) 
     (if (singleton? m) 
         (car m) 
         m)))



;;; auxiliary accumulate
(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (smallest-op expr) 
   (accumulate (lambda (a b) 
                 (if (operator? a) 
                     (min-precedence a b) 
                     b)) 
               'maxop 
               expr)) 

(define *precedence-table* 
   '( (maxop . 10000) 
      (minop . -10000) 
      (+ . 0) 
      (* . 1) )) 

(define (operator? x) 
   (define (loop op-pair) 
     (cond ((null? op-pair) #f) 
           ((eq? x (caar op-pair)) #t) 
           (else (loop (cdr op-pair))))) 
   (loop *precedence-table*))

(define (min-precedence a b) 
   (if (precedence<? a b) 
       a 
       b))

(define (precedence<? a b) 
   (< (precedence a) (precedence b)))

(define (precedence op) 
   (define (loop op-pair) 
     (cond ((null? op-pair) 
            (error "Operator not defined -- PRECEDENCE:" op)) 
           ((eq? op (caar op-pair)) 
            (cdar op-pair)) 
           (else 
            (loop (cdr op-pair))))) 
   (loop *precedence-table*))

(define (singleton? a)
    (and (pair? a) (null? (cdr a))))

(define (prefix sym list) 
   (if (or (null? list) (eq? sym (car list))) 
       '() 
       (cons (car list) (prefix sym (cdr list)))))



;;; test

(deriv '(x + 3 * (x + y + 2)) 'x)
;Value: 4

(deriv '(x + 3) 'x)
;Value: 1

(deriv '(x * y * (x + 3)) 'x)
;Value 88: ((x * y) + (y * (x + 3)))

;; Will extraneous parens throw our deriv for a loop?
(deriv '((x * y) * (x + 3)) 'x)
;Value 89: ((x * y) + (y * (x + 3)))

(deriv '(x * (y * (x + 3))) 'x)
;Value 90: ((x * y) + (y * (x + 3)))










