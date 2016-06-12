
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)


(define z (make-cycle (list 'a 'b 'c)))

(define (find-end x)
    (if (not (pair? x))
        'done
        (find-end (cdr x))))


(define (cycle? x) 
  (define nil '())
  (define visited nil) 
  (define (iter x) 
    (set! visited (cons x visited)) 
    (cond ((null? (cdr x)) false) 
          ((memq (cdr x) visited) true) 
          (else (iter (cdr x))))) 
  (iter x)) 




