(define (apply-generic op . args) 
   ; coercing list to a type 
   (define (coerce-list-to-type lst type) 
     (if (null? lst)  
       '() 
       (let ((t1->t2 (get-coercion (type-tag (car lst)) type))) 
         (if t1->t2 
           (cons (t1->t2 (car lst)) (coerce-list-to-type (cdr lst) type)) 
           (cons (car lst) (coerce-list-to-type (cdr lst) type)))))) 
  
   ; applying to a list of multiple arguments 
   (define (apply-coerced lst) 
     (if (null? lst) 
       (error "No method for given arguments") 
       (let ((coerced-list (coerce-list-to-type args (type-tag (car lst))))) 
         (let ((proc (get op (map type-tag coerced-list)))) 
           (if proc 
             (apply proc (map contents coerced-list)) 
             (apply-coerced (cdr lst))))))) 
  
   ; logic to prevent always coercing if there is already direct input entry 
   (let ((type-tags (map type-tag args))) 
     (let ((proc (get op type-tags))) 
       (if proc 
         (apply proc (map contents args)) 
         (apply-coerced args))))) 



;;; Give an example of a situation where this strategy 
;;; (and likewise the two-argument version given above) 
;;; is not sufficiently general. (Hint: Consider the case 
;;; where there are some suitable mixed-type operations 
;;; present in the table that will not be tried.)







