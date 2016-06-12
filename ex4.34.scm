
(load "/home/lancecopper/code/SICP/old-eval-apply.scm")
(load "/home/lancecopper/code/SICP/lcons-env-primitive.scm")
(load "/home/lancecopper/code/SICP/new-analyze.scm")


(define (text-of-quotation exp)
    (define (make-lcons-list expr)
        (if (null? expr)
            '()
            (lcons (car expr) (make-lcons-list (cdr expr)))))
    (let ((expr (cadr exp)))
         (make-lcons-list expr)))




(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))


(define (lcons-disp obj)
    (let ((car-obj (lcar obj))
          (cdr-obj (lcdr obj)))
         (cons
             (if (lcons? car-obj)
                 '<lazy-list>
                 car-obj)
             (if (lcons? cdr-obj)
                 '<lazy-list>
                 cdr-obj))))

(define (lcons? obj)
    (tagged-list? obj 'lcons))

(define (user-print object)
  (cond ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        ((lcons? object)
         (display (lcons-disp object)))
        (else (display object))))

;;; test
(driver-loop)
(define foo (cons (cons 1 2) (cons 3 4)))

