
  
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





(define (analyze exp)
(cond
;...
    ((let? exp) (analyze (let->combination exp)))
;...


(define)




