;; To accomodate generic number in the complex package,  
 ;; we should replace operators such as + , * with theirs 
 ;; generic counterparts add, mul. 
 (define (add-complex z1 z2) 
   (make-from-real-imag (add (real-part z1) (real-part z2)) 
                        (add (imag-part z1) (imag-part z2)))) 
 (define (sub-complex z1 z2) 
   (make-from-real-imag (sub (real-part z1) (real-part z2)) 
                        (sub (imag-part z1) (imag-part z2)))) 
 (define (mul-complex z1 z2) 
   (make-from-mag-ang (mul (magnitude z1) (magnitude z2)) 
                      (add (angle z1) (angle z2)))) 
 (define (div-complex z1 z2) 
   (make-from-mag-ang (div (magnitude z1) (magnitude z2)) 
                      (sub (angle z1) (angle z2))




;;; add into global 
(define (sine x) (apply-generic 'sine x)) 
(define (cosine x) (apply-generic 'cosine x)) 
(define (arctan x) (apply-generic 'arctan x)) 
(define (exp x y) (apply-generic 'exp x y)) 

;;; add into rational package  
(put 'sine '(number) (lambda (x) (tag (sin x)))) 
(put 'cosine '(number) (lambda (x) (tag (cos x)))) 
(put 'arctan '(number) (lambda (x) (tag (atan x)))) 
(put 'exp '(number number) (lambda (x y) (tag (expt x y)))) 

; add into scheme-number package 
(put 'sine 'scheme-number 
  (lambda (x) (sin x))) 
(put 'cosine 'scheme-number 
  (lambda (x) (cos x))) 


(put 'sine 'rational (lambda (x) (sin (/ (numer x) (denom x)))))  
(put 'cosine 'rational (lambda (x) (cos (/ (numer x) (denom x))))) 
