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












