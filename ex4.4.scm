
;;; a
((and? expr) (eval-and (and-clauses expr) env)) 
((or? expr) (eval-or (or-clauses expr) env)) 


(define (and? expr) (tagged-list? expr 'and)) 
(define (and-clauses expr) (cdr expr))
(define (eval-and exprs env) 
        (let ((v (eval (first-exp exprs) env))) 
             (if (last-exp? exprs)
                 (if v v #f) 
                 (if v
                     (eval-and (rest-exps exprs) env) 
                     #f))))


(define (or? expr) (tagged-list? expr 'or)) 
(define (or-clauses expr) (cdr expr))
(define (eval-or exprs env)
        (let ((v (eval (first-exp exprs) env)))
             (if (last-exp? exprs)
                 (if v v #f)
                 (if v 
                     v
                     (eval-or (rest-exps exprs) env)))))



;; b. you can use nested if implemented "and" and "or", like this: 
((and? expr) (eval (and->if expr) env)) ;; add in eval 
 
(define (and->if expr) 
        (expand-and-clauses (and-clauses expr))) 
(define (expand-and-clauses clauses)      
    (let ((first (car clauses)) 
          (rest (cdr clauses))) 
         (if (null? rest)  
             (make-if first first 'false) 
             (make-if first (expand-and-clauses rest) 'false)))) 
 

((or? expr) (eval (or->if expr) env))  ;; add in eval 
(define (or->if expr) 
        (expand-or-clauses (or-clauses expr))) 
(define (expand-or-clauses clauses)
    (let ((first (car clauses)) 
          (rest (cdr clauses)))
         (if (null? rest)
             (make-if first first 'false)
             (make-if first first (expand-or-clauses rest)))))


