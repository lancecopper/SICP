(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)


(define z (make-cycle (list 'a 'b 'c 'd 'e 'f)))

(define (find-end x)
    (if (not (pair? x))
        'done
        (find-end (cdr x))))


(define (contains-cycle? lst) 
  (define (safe-cdr l) 
    (if (pair? l) 
        (cdr l) 
        '())) 
  (define (iter a b)
     (begin
       (display (car a))
       (newline)
       (display (car b))
       (newline)   
       (cond ((not (pair? a)) #f) 
             ((not (pair? b)) #f) 
             ((eq? a b) #t) 
             ((eq? a (safe-cdr b)) #t) 
             (else (iter (safe-cdr a) (safe-cdr (safe-cdr b))))))) 
  (iter (safe-cdr lst) (safe-cdr (safe-cdr lst))))

;;;
(contains-cycle? z)


(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)


(define z (make-cycle (list 'a 'b 'c 'd 'e 'f)))


(define (cycle-const-space? x) 
  (define (iter x cont elem num)
    (begin
      (display (car x))
      (newline)
      (display (car elem))
      (newline)  
      (cond ((null? (cdr x)) false) 
            ((eq? x elem) true) 
            (else (if (= cont num) 
                    (iter (cdr x) 0 x (+ 1 num)) 
                    (iter (cdr x) (+ cont 1) elem num)))))) 
  (iter x 0 '(nil nil) 0))


;;;
(contains-cycle? z)
(cycle-const-space? z) 




