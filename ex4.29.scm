

;;; test1
(define (test exp)
    (let ((t0 0) (t1 0))
        (define (loop i n)
            (+ exp 1)
            (if (< i n)
                (loop (+ i 1) n)))
        (set! t0 (get-universal-time))
        (loop 0 100)
        (set! t1 (get-universal-time))
        (- t1 t0)))

(define (fib i) 
    (if (< i 3) 
        1 
        (+ (fib (- i 1)) (fib (- i 2))))) 




(driver-loop)

(test (fib 10))


;;; test2

;;; id

(define count 0)
(define (id x)
  (set! count (+ count 1))
  x)


(define (square x)
  (* x x))
;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
100
;;; L-Eval input:
count
;;; L-Eval value:
1


