;;; a

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (= '*unassigned* (car vals))
                (error "unassigned variable" var)
                (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;;; b

(define (scan-out-defines body)
  (let ((let-body '())
        (set-body '())
        (others '()))
    (let scan-iter ((b body))
      (cond ((null? b)
             '())
            ((definition? (car b))
             (let ((def-var (definition-variable (car b)))
                   (def-val (definition-value (car b))))
               (set! let-body (cons (list def-var ''*unassigned*) 
                                    let-body))
               (set! set-body (cons (cons 'set! (list def-var def-val))
                                    set-body))))
            (else (set! others (append others (list (car b))))))
      (if (not (null? b))
          (scan-iter (cdr b))))
    (if (null? let-body)
        body
        (list (append (list 'let let-body) (append set-body others))))))


;;;
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


;;; c
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

;(define (procedure-body p) (scan-out-defines (caddr p)))
