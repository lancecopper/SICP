(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) 
    (cond ((null? set) (list x)) 
          ((= x (car set)) set) 
          ((< x (car set)) (cons x set)) 
          (else (cons (car set) (adjoin-set x (cdr set))))))

(define (adjoin-set x set)
    (define (iter early rest)
        (cond ((null? rest) (append early (list x)))
              ((= (car rest) x) (append early rest))
              ((> (car rest) x) (append early (cons x rest)))
              (else (iter (append early (list (car rest))) (cdr rest)))))
    (iter '() set))



(define (union-set set1 set2)
    (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2))
            (cons (car set1) (union-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2))
            (cons (car set1) (union-set (cdr set1) set2)))
        (else (cons (car set2) (union-set set1 (cdr set2))))))

(define (union-set set1 set2)
    (define (iter s1 s2 result)
        (cond ((null? s1) (append result s2))
              ((null? s2) (append result s1))
              ((= (car s1) (car s2))
                (iter (cdr s1) (cdr s2) (append result (list (car s1)))))
              ((< (car s1) (car s2))
                (iter (cdr s1) s2 (append result (list (car s1)))))
              (else
                (iter s1 (cdr s2) (append result (list (car s2)))))))
    (iter set1 set2 '()))


;;; test
(define set1 '(1 2 3 4 5 6 7 8 9 10))
(define set2 '(2 4 6 8 10 12 14 16 18 20))




























