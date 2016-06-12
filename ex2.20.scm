(define (reverse items)
    (define (iter items items1)
        (if (null? items)
            items1
            (iter (cdr items) (cons (car items) items1))))
    (iter items ()))

(define (filter-items filter items)
    (define (iter items items1)
        (if (null? items)
            items1
            (if (filter (car items))
                (iter (cdr items) (cons (car items) items1))
                (iter (cdr items) items1))))
    (reverse (iter items ())))

(define (same-parity x . y)
    (cond ((null? x) ())
        ((null? y) x)
        (else
            (if (odd? x)
                (cons x (filter-items odd? y))
                (cons x (filter-items even? y))))
    ))