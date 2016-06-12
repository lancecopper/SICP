
;;; You will have to implement new syntax procedures 
;;; to handle the new syntax for define. 

;;; You must also arrange for eval or apply 
;;; to determine when arguments are to be delayed, 

;;; and to force or delay arguments accordingly, 
;;; and you must arrange for forcing to memoize 
;;; or not, as appropriate.


(load "/home/lancecopper/code/SICP/lazy-evaluation.scm")

;;; thunk
(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

;;; thunk-memo
(define (delay-memo-it exp env)
  (list 'thunk-memo exp env))

(define (thunk-memo? obj)
  (tagged-list? obj 'thunk-memo))



;;; evaluated-thunk
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

;;;
(define (force-it obj)
  (cond ((thunk? obj)
         (actual-value (thunk-exp obj) (thunk-env obj)))
        ((thunk-memo? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)  ; replace exp with its value
           (set-cdr! (cdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))



(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)
          ))
        ((compound-procedure? procedure) 
             (eval-compound-procedure procedure arguments env)) 
         (else 
             (error "Unknown procedure type -- APPLY" procedure)))) 


(define (eval-compound-procedure procedure arguments env) 
    (define (iter-args formal-args actual-args) 
        (if (null? formal-args) 
            '() 
            (cons 
                (let ((this-arg (car formal-args))) 
                    (cond ((and (pair? this-arg) 
                             (pair? (cdr this-arg))
                             (eq? (cadr this-arg) 'lazy))
                           (delay-it (car actual-args) env))
                          ((and (pair? this-arg) 
                             (pair? (cdr this-arg))
                             (eq? (cadr this-arg) 'lazy-memo))
                           (delay-memo-it (car actual-args) env)) 
                         ;force the argument if it is not lazy.  
                         (else (actual-value (car actual-args) env)))) 
                (iter-args (cdr formal-args) (cdr actual-args)))))
    (define (procedure-arg-names parameters) 
        (map (lambda (x) (if (pair? x) (car x) x)) parameters))
    (eval-sequence 
        (procedure-body procedure) 
        (extend-environment 
            (procedure-arg-names (procedure-parameters procedure)) 
            (iter-args  
                (procedure-parameters procedure) 
                arguments) 
            (procedure-environment procedure))))


;; test 
  
(driver-loop)

(define (f a b)
    (+ a b))
(f 1 2)

(define (f1 a (b lazy))
    (+ a b))
(f1 1 2)

(define (f2 a (b lazy-memo))
    (+ a b))
(f2 1 2)


(define (f3 a (b lazy) c (d lazy-memo))
    (cons
        (+ a c)
        (+ a b c d)))

(define (fibonacci n)
    (if (or (= n 1) (= n 2))
        1
        (+ (fibonacci (- n 1))
           (fibonacci (- n 2)))))

(define x 1) 

(f3 0.001 (begin (set! x (cons x '(2))) 20000) 100000 (fibonacci 15))
 


; M-Eval input:  
(define x 1) 
(define (p (e lazy)) e x)  
 
(p (set! x (cons x '(2))))


(define (p (e lazy))
    e x)
(p (set! x (cons x '(2))))

