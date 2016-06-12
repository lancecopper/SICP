
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
    (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
            (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))


;;; test
(define set1 '(1 2 3 4 5))
(define set2 '(2 4 6 8 10))

(intersection-set set1 set2)

(union-set set1 set2)




(define (element-of-set? x set) 
   (cond ((null? set) false) 
         ((equal? x (car set)) true) 
         (else (element-of-set? x (cdr set))))) 
  
(define (adjoin-set x set) 
    (cons x set)) 
  
(define (union-set set1 set2) 
    (append set1 set2)) 

(define (remove-set-element x set) 
    (define (remove-set-element-iter acc rest) 
        (cond ((null? rest) acc) 
            ((equal? x (car rest)) (append acc (cdr rest))) 
            (else (remove-set-element-iter (adjoin-set (car rest) acc) (cdr rest))))) 
    (remove-set-element-iter '() set)) 
  
(define (intersection-set set1 set2) 
    (cond ((or (null? set1) (null? set2)) '()) 
        ((element-of-set? (car set1) set2) 
            (cons (car set1) 
                (intersection-set (cdr set1) (remove-set-element (car set1) set2)))) 
            (else (intersection-set (cdr set1) set2)))) 

;;; test

(define set1 (list 2 3 2 1 3 2 2))
(define set2 (list 2 3 3 2 2))
















