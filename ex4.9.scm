(define (while? expr) (tagged-list? expr 'while))
(define (while-condition) (cadr expr))
(define (while-body) (caddr expr))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (while->if expr)
    (make-if (while-condition expr) (while-body expr) 'done))




(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((while? exp) (eval (while->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))



;;; test
(define (add-to-10 a)
    (while (< a 10)
        (set! a (+ a 1)))
    a)

(while? '(while (< a 10)
        (set! a (+ a 1))))
