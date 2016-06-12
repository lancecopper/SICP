(define (eval exp env)
    (cond
        ...
        ((unless? exp) (eval (unless->if exp) env))
        ... 
    ))


; ('unless <condition-exp> <usual-value-exp> <exceptional-value-exp>)

(define (unless? exp)
    (tagged-list? exp 'unless))

(define (unless-condition exp)
    (cadr exp))

(define (unless-usual-value exp)
    (caddr exp))

(define (unless-exceptional-value exp)
    (cadddr exp))

(define (unless->if exp)
    (let ((predicate (unless-condition exp))
          (consequent (unless-exceptional-value exp))
          (alternative (unless-usual-value exp)))
         (make-if predicate consequent alternative)))


;;; test
(unless (= b 0)
        (/ a b)
        (begin (display "exception: returning 0")
               0))

(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))






