(define (reverse items)
    (define (iter-1 items items1)
        (if (null? items)
            items1
            (iter-1 (cdr items) (cons (car items) items1))))
    (iter-1 items ()))


(define (fringe items)
    (define (iter items result)
        (if (null? items)
            result 
            (iter (cdr items)
                  (if (not (pair? (car items)))
                      (cons (car items) result)
                      (append (iter (car items) ()) result)))))
    (reverse (iter items ())))

(define (fringe tree) 
   (define nil '()) 
   (if (null? tree)  
       nil 
       (let ((first (car tree))) 
         (if (not (pair? first)) 
             (cons first (fringe (cdr tree))) 
             (append (fringe first) (fringe (cdr tree)))))))

(define (fringe tree) 
   (define nil '())
   (define (iter tree result)
    (if (null? tree)
        result
        (let ((first (car tree)))
            (if (not (pair? first))
                (iter (cdr tree) (cons first result))
                (iter (cdr tree) (append (iter first nil) result)))
        )))
   ((reverse (iter tree nil)))



(define x (list (list 1 2) (list 3 4)))

(fringe x)
(1 2 3 4)

(fringe (list x x))
(1 2 3 4 1 2 3 4)

