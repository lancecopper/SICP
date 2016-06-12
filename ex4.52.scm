(load "/home/lancecopper/code/SICP/amb-analyze.scm")

(driver-loop)


;;; auxiliary function
(define (ramb-list words)
    (if (null? words)
        (ramb)
        (ramb (car words)
             (ramb-list (cdr words)))))

(define (an-element-of exprs)
    (ramb-list exprs))

(define (require p)
  (if (not p) (amb)))


;;; Amb-Eval input:
(if-fail (let ((x (an-element-of '(1 3 5))))
           (require (even? x))
           x)
         'all-odd)
;;; Starting a new problem
;;; Amb-Eval value:
all-odd
;;; Amb-Eval input:
(if-fail (let ((x (an-element-of '(1 3 5 8))))
           (require (even? x))
           x)
         'all-odd)
;;; Starting a new problem
;;; Amb-Eval value:
8












