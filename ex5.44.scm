(define (overwrite? operator ct-env) 
  (let ((r (find-variable operator ct-env))) 
    (eq? r 'not-found)))

(define (open-code? exp ct-env) 
   (and (memq (car exp) '(+ - * /)) 
        (overwrite? (car exp) ct-env)))

;;; in reference with ex5.48-compiler.scm



