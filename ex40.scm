(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (permutations s)
  (if (null? s)                    ; empty set?
      (list nil)                   ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))


(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))



(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))


(define (enumerate-interval x y)
    (if (> x y)
        nil
        (cons x (enumerate-interval (+ x 1) y))))

(define (enumerate-interval x y)
    (define (iter x y result)
        (if (> x y)
            result
            (iter (+ x 1) y (cons x result))))
    (reverse (iter x y nil)))


(define (unique-pair n)
    (let ((proc (lambda (i) 
                        (map (lambda (j) (list i j)) 
                             (enumerate-interval 1 (- i 1)))))
          (seq (enumerate-interval 1 n)))
    (flatmap proc seq)))


(define (prime-sum-pairs n)
    (map make-pair-sum
       (filter prime-sum? (unique-pair n))))

(define (prime? n) 
    (if (< n 2) false
        (= n (smallest-divisor n)))) 

(define (smallest-divisor n) 
    (find-divisor n 2))


(define (find-divisor n test-divisor) 
    (cond ((> (square test-divisor) n) n) 
         ((divides? test-divisor n) test-divisor) 
         (else (find-divisor n (next test-divisor)))))

(define (divides? a b) 
    (= (remainder b a) 0)) 

(define (next x)
    (if (=  x 2) 3
        (+ x 2)))


(prime-sum-pairs 10)