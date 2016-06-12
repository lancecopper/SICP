(load "/home/lancecopper/code/SICP/amb-analyze.scm")

(driver-loop)





;;; require

(define (require-predicate exp) (cadr exp))

(define (require? exp) (tagged-list? exp 'require))

(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (not (true? pred-value))
                   (fail)
                   (succeed 'ok fail2)))
             fail))))
















