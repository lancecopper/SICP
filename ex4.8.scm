


(define (let? expr) (tagged-list? expr 'let))
(define (let-symbol expr)
    (if (pair? (cadr expr))
        '()
        (cadr expr)))
(define (let-bindings expr)
    (if (null? (let-symbol expr))
        (cadr expr)
        (caddr expr)))
(define (let-body expr)
    (if (null? (let-symbol expr))
        (cddr expr)
        (cdddr expr)))
(define (let-vars expr) (map car (let-bindings expr))) 
(define (let-inits expr) (map cadr (let-bindings expr))) 


(define (let->combination expr)
  (list 
    (make-lambda 
        (cons 
            (let-symbol expr) 
            (let-vars expr)) 
        (let-body expr)) 
    (cons 
        (make-lambda 
            (let-vars expr) 
            (let-body expr)) 
        (let-inits expr)))) 










