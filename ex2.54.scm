(define (equal? l1 l2)
    (if (and (pair? l1) (pair? l2))
        (and (eq? (car l1) (car l2)) (equal? (cdr l1) (cdr l2)))
        (if (and (not (pair? l1)) (not (pair? l2)))
            (eq? l1 l2)
            false)))

(equal? '(this (is a) list) '(this (is a) list))
(eq? '(this (is a) list) '(this (is a) list))





