(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))


(define (make-unbound! var env)
    (let ((frame (first-frame env)))
        (define (scan vars vals)
            (cond ((null? vars)
                   (cons '() '()'))
                  ((eq? var (car vars))
                   (cons (cdr vars) (cdr vals)))
                  (else 
                   (let ((result (scan (cdr vars) (cdr vals))))
                        (cons (cons (car vars) (car result))
                              (cons (car vals) (cdr result)))))))
        (scan (frame-variables frame)
              (frame-values frame))))
