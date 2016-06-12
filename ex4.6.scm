  
;; in eval, add this: 
((let? expr) (evaln (let->combination expr) env)) 

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; let expression 
(define (let? expr) (tagged-list? expr 'let)) 
(define (let-vars expr) (map car (cadr expr))) 
(define (let-inits expr) (map cadr (cadr expr))) 
(define (let-body expr) (cddr expr)) 
 
(define (let->combination expr) 
  (list (make-lambda (let-vars expr) (let-body expr)) 
        (let-inits expr))) 




(define (eval exp env)
; ......
        ((let? exp) (eval (let->combination exp) env))
; ......
)

(define (let? exp) (tagged-list? exp 'let))
(define (let-assignment exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let-exp assignment)
  (if (null? assignment)
      '()
      (cons (cadr (car assignment))
            (let-exp (cdr assignment)))))
(define (let-var assignment)
  (if (null? assignment)
      '()
      (cons (car (car assignment))
            (let-var (cdr assignment)))))
  
(define (let->combination exp)
  (transform-let (let-assignment exp) (let-body exp)))
(define (transform-let assignment body)
  (cons (make-lambda (let-var assignment) body)
        (let-exp assignment)))


;;;

;;; let

(define (let? expr) (tagged-list? expr 'let)) 
(define (let-vars expr) (map car (cadr expr))) 
(define (let-inits expr) (map cadr (cadr expr))) 
(define (let-body expr) (cddr expr)) 
 
(define (let->combination expr)
    (transform-let (let-vars expr) (let-inits expr) (let-body expr)))

(define (transform-let vars inits body)
    (cons (make-lambda vars body)
          inits))