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

(define (samllest-divisor n)
    (find-divisor n 2))

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))


(define (divides? a b)
    (= (remainder b a) 0))

(define (prime? n)
    (= n (samllest-divisor n)))

(define (prime-sum-pair ls1 ls2)
    (let ((e1 (an-element-of ls1))
          (e2 (an-element-of ls2)))
        (require (prime? (+ e1 e2)))
        (cons e1 e2)))


(let ((pairs '()))
    (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
             (permanent-set! pairs (cons p pairs))
             (amb))
           pairs))








(define (require p)
  (if (not p) (amb)))




