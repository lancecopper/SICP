(define (install-raise)
    (define (raise-sche x)
        (make-rat x 1))
    (define (raise-rat x)
        (make-real (/ (numer x) (denom x))))
    (define (raise-real x)
        (make-from-real-imag x 0))
    (put 'raise '(scheme-number) raise-sche)
    (put 'raise '(rational) raise-rat)
    (put 'raise '(complex) raise-real)
    'done)

(define (raise x)
    (apply-generic 'raise x))







































