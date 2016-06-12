(define (install-equ)
    (define (equ-sche-sche x y)
        (= x y))
    (define (equ-rat-rat x y)
        (and (= (numer x) (numer y)) (= (denom x) (denom y))))
    (define (equ-com-com x y)
        (and (= (real-part x) (real-part y))
             (= (imag-part x) (imag-part y))))
    (put 'equ '(scheme-number scheme-number) equ-sche-sche)
    (put 'equ '(rational rational) equ-rat-rat)
    (put 'equ '(real real) equ-sche-sche)
    (put 'equ '(complex complex) equ-com-com))

(define (equ? x y)
    (if (not (eq? (type-tag x) (type-tag y)))
        false
        (apply-generic 'equ x y)))

























