(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))


(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

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

(define (unique-triple n)
    (let ((proc (lambda (i) 
                        (flatmap (lambda (j) (map (lambda (k) (list i j k))
                                               (enumerate-interval 1 (- j 1))))
                             (enumerate-interval 1 (- i 1)))))
          (seq (enumerate-interval 1 n)))
    (flatmap proc seq)))

(define (s-sum-triple n s)
    (filter (lambda (triple) (= s (sum triple))) 
            (unique-triple n)))

(define (sum l)
    (define (iter l result)
        (if (null? l)
            result
            (iter (cdr l) (+ result (car l)))
            ))
    (iter l 0))

(define nil '())

(s-sum-triple 10 22)





