

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))


;;; A. assuming that good-enough? and improve are primtives 
(controller
    (assign x (op read)) 
    (assign guess (const 1.0))

 test-good
    (test (op good-enough?) (reg guess) (reg x))
    (branch (label sqrt-done))
    (assign t (op improve) (reg guess) (reg x))
    (assign guess (reg t))
    (goto (label test-good))
 sqrt-done)


;; B1. inline improve 
 
(controller 
    (assign x (op read)) 
    (assign guess (const 1.0)) 
 
 test-good 
    (test (op good-enough?) (reg guess) (reg x)) 
    (branch (label sqrt-done)) 
 
    ;; improve procedure 
    (assign div (op /) (reg x) (reg guess)) 
    (assign avg (op average) (reg guess) (reg div)) 
    
    (assign t (reg avg)) 
    (assign guess (reg t)) 
    (goto label test-good) 
 
 sqrt-done 
    (perform (op print) (reg guess)) 
)


;;; B2. inline average 
(controller 
    (assign x (op read)) 
    (assign guess (const 1.0)) 
    
 test-good 
    (test (op good-enough?) (reg guess) (reg x)) 
    (branch (label done)) 
    
    ;; improve procedure 
    (assign div (op /) (reg x) (reg guess)) 
    
    ;; average procedure 
    (assign sum (op +) (reg guess) (reg div)) 
    (assign avg (op /) (reg sum) (const 2)) 
    
    (assign t (reg avg)) 
    (assign guess (reg t)) 
    (goto label test-good) 
 
 sqrt-done   
    (perform (op print) (reg guess)) 
) 


;;; B3. inline good-enough? 
(controller 
    (assign x (op read)) 
    (assign guess (const 1.0)) 
    
 test-good  
    ;; good-enough? procedure 
    (assign square (op *) (reg guess) (reg guess)) 
    (assign diff (op -) (reg square) (reg x)) 
    (test (op <) (reg diff) (const 0)) 
    (branch test-abs-neg) 
 
 test-abs-pos 
    (assign abs (reg diff)) 
 
 test-abs-neg 
    (assign abs (op *) (reg diff) (const -1)) 
    
    (test (op <) (reg abs) (const 0.001)) 
    
    (branch (label done))
    
    ;; improve procedure 
    (assign div (op /) (reg x) (reg guess)) 
    ;; average procedure 
    (assign sum (op +) (reg guess) (reg div)) 
    (assign avg (op /) (reg sum) (const 2)) 
    
    (assign t (reg avg)) 
    (assign guess (reg t)) 
    (goto test-good) 
 
 sqrt-done
    (perform (op print) (reg guess))
) 





(controller
    (assign x (op read)) 
    (assign guess (const 1.0))

 test-good
    (assign t1 (op *) (reg guess) (reg guess))
    (assign t2 (op -) (reg t1) (reg x))
    (test (op <) (reg t2) (const 0.001))
    (branch (label sqrt-done))
    (assign t1 (op /) (reg x) (reg guess))
    (assign t2 (op +) (reg guess) (reg t1))
    (assign t1 (op /) (reg t2) (const 2))
    (assign guess (reg t1))
    (goto (label test-good))
 sqrt-done)


