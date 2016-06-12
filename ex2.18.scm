(define (reverse items)
    (define (iter items items1)
        (if (null? items)
            items1
            (iter (cdr items) (cons (car items) items1))))
    (iter items (list)))
