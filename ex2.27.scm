(define (reverse items)
    (define (iter items items1)
        (if (null? items)
            items1
            (iter (cdr items) (cons (car items) items1))))
    (iter items ()))

(define (deep-reverse items)
    (define (iter items items1)
        (if (null? items)
            items1
            (iter (cdr items) 
                  (cons (if (not (pair? (car items))) 
                        (car items) 
                        (deep-reverse (car items)))
                  items1))))
    (iter items ()))



(define x (list (list 1 2) (list 3 4)))

x
((1 2) (3 4))

(reverse x)
((3 4) (1 2))

(deep-reverse x)
((4 3) (2 1))




