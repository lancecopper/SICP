((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10)

;;; a

((lambda (n)
    ((lambda (fib)
        (fib fib n))
    (lambda (ft k)
        (cond ((= k 1) 1)
              ((= k 2) 1)
              (else (+ (ft ft (- k 1)) (ft ft (- k 2))))))))
 10)

(define 
    fib
    (lambda (n)
    ((lambda (fib)
        (fib fib n))
    (lambda (ft k)
        (cond ((= k 1) 1)
              ((= k 2) 1)
              (else (+ (ft ft (- k 1)) (ft ft (- k 2)))))))))

(define (fibs n)
    (let iter 
         ((k 1))
         (if (< n k)
             'done
             (begin
                 (newline)
                 (display (fib k))
                 (set! k (+ k 1))
                 (iter k)))))
;;; b


(define (f x)
  (define (even? n)
    (if (= n 0)
        true
        (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0)
        false
        (even? (- n 1))))
  (even? x))


(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))



