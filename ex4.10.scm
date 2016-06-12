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


;;; It's easy to switch the order of let-bingdings and let-body
;;; of let expressiong like this: 
;;; (let <let-body> (<let-var1> <let-init1>) (<let-var2> <let-init2>) ...)
  

;; in eval, add this: 
((let? expr) (eval (let->combination expr) env)) 

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; let expression 
(define (let? expr) (tagged-list? expr 'let)) 
(define (let-vars expr) (map car (cddr expr))) 
(define (let-inits expr) (map cadr (cddr expr))) 
(define (let-body expr) (cadr expr)) 
 
(define (let->combination expr) 
  (list (make-lambda (let-vars expr) (let-body expr)) 
        (let-inits expr))) 








