(load "/home/lancecopper/code/SICP/explicit-control-evaluator-adjuvant.scm")

(define (application-with-symbol-operator? exp)
    (and (application? exp)
         (variable? (operator exp))))

;;; application
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define eceval-operations
    (append eceval-operations
        (list 
            (list 'application-with-symbol-operator?
                  application-with-symbol-operator?))))





