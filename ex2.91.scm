(define (div-terms L1 L2) 
  (if (empty-termlist? L1) 
    (list (the-empty-termlist) (the-empty-termlist)) 
    (let ((t1 (first-term L1)) 
          (t2 (first-term L2))) 
      (if (> (order t2) (order t1)) 
        (list (the-empty-termlist) L1) 
        (let ((new-c (div (coeff t1) (coeff t2))) 
          (new-o (- (order t1) (order t2))) 
          (new-t (make-term new-o new-c))) 
          (let ((rest-of-result 
                  (div-terms (add-terms L1 (negate (mul-terms (list new-t) L2))) 
                              L2))) 
            (list (adjoin-term new-t 
                               (car rest-of-result)) 
                  (cadr rest-of-result)))))))) 


 
(define (div-poly p1 p2) 
  (if (same-variable? (variable p1) (variable p2)) 
    (let ((results (div-terms (term-list p1) 
                              (term-list p2)))) 
      (list (make-poly (variable p1) (car results)) 
            (make-poly (variable p1) (cadr results)))) 
    (error "Polys not in same var -- div-poly" 
           (list p1 p2)))) 





















