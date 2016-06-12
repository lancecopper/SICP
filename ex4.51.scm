;;; new eval
(load "/home/lancecopper/code/SICP/amb-analyze.scm")

(driver-loop)

(define (require p)
  (if (not p) (amb)))

(define (ramb-list words)
    (if (null? words)
        (ramb)
        (ramb (car words)
             (ramb-list (cdr words)))))


(define count 0)
(let ((x (ramb-list '(a b c)))
      (y (ramb-list '(a b c))))
  (permanent-set! count (+ count 1))
  (require (not (eq? x y)))
  (list x y count))



(let ((x (ramb-list '(a b c)))
      (y (ramb-list '(a b c))))
  (set! count (+ count 1))
  (require (not (eq? x y)))
  (list x y count))



;;; Starting a new problem
;;; Amb-Eval value:
(a b 2)
;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
(a c 3)





