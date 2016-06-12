;;; old eval-sequence

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))


;;; Cy D. Fect

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;;; a

(define (for-each proc items)
  (if (null? items)
      'done
      (begin (proc (car items))
             (for-each proc (cdr items)))))

;;; L-Eval input:
(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))


57
321
88
;;; L-Eval value:
done






;;; b

(driver-loop)


(define (p1 x)
  (set! x (cons x '(2)))
  x)


(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))

(p1 1)
(p2 1)









