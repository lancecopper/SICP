(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
    (define (iter variables values)
        (if (not (null? variables))
            (cons (cons (car variables) (car values))
                  (iter (cdr variables) (cdr values)))
            '()))
    (if (= (length variables) (length values))
        (iter variables values)
        (if (< (length varibles) (length values))
          (error "Too many values supplied" values)
          (error "Too many varibles supplied" varibles))))

(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))

(define (add-binding-to-frame! var val frame)
  (set! frame (cons (cons var val) frame)))


(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))


(define (lookup-variable-value var env)
    (define (env-loop env)
        (define (scan frame)
            (cond ((null? frame)
                   (env-loop (enclosing-environment env)))
                  ((eq? var (caar frame))
                   (caar frame))
                  (else (scan (cdr frame)))))
        (if (eq? env the-empty-environment)
            (error "Unbound variable" var)
            (let ((frame (first-frame env)))
                 (scan frame))))
    (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar frame))
             (set-cdr! (car frame) val))
            (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))


(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan frame)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (caar frame))
             (set-cdr! (car frame) val))
            (else (scan (cdr frame)))))
    (scan frame)))








