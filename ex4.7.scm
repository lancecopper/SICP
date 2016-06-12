(define (let*? expr) (tagged-list? expr 'let*)) 
(define (let*-inits expr) (cadr expr) 
(define (let*-body expr) (cddr expr)) 


(define (let*->nested-lets expr)
    (let ((inits (let*-inits expr))
          (body (let*-body expr)))
         (define (make-lets inits)
            (if (null? inits)
                body
                (list 
                    'let 
                    (list (car inits))
                    (make-lets (cdr inits)))))
         (make-lets inits)))
