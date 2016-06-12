;;; To make the interpreter implement the “simultaneous” 
;;; scope rule for internal definitions without 
;;; constructing the extra frame, we can rearrange 
;;; the order of the procedure body making sure
;;; internal definitions always appear before being called.

(define (rearrange-defines body)
  (let ((defs '())
        (others '()))
    (let scan-iter ((b body))
      (cond ((null? b)
             '())
            ((definition? (car b))
             (set! defs (append defs (list (car b))))) 
            (else 
             (set! others (append others (list (car b))))))
      (if (not (null? b))
          (scan-iter (cdr b))))
    (if (null? defs)
        body
        (append defs others))))

(define (make-procedure parameters body env)
  (list 'procedure parameters (rearrange-defines body) env))







