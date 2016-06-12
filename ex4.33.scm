(load "/home/lancecopper/code/SICP/lazy-evaluation.scm")


(define (lcons x y)
  (lambda (m) (m x y)))
(define (lcar z)
  (z (lambda (p q) p)))
(define (lcdr z)
  (z (lambda (p q) q)))


(define (text-of-quotation exp)
    (define (make-lcons-list expr)
        (if (null? expr)
            '()
            (lcons (car expr) (make-lcons-list (cdr expr)))))
    (let ((expr (cadr exp)))
         (make-lcons-list expr)))

;;; modify old-env-primitive.scm
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        ... ...

;;; ====>>>>
(define primitive-procedures
  (list (list 'car lcar)
        (list 'cdr lcdr)
        (list 'cons lcons)
        ... ...


(car '(a b c))






