(define (install-=zero?)
    (define (=zero?-sche x)
        (= x 0))
    (define (=zero?-rat x)
        (= (numer x) 0))
    (define (=zero?-com x)
        (and (= (real-part x) 0)
             (= (imag-part x) 0)))
    (define (=zero?-poly x)
        (=zero? (coeff x)))
    (put '=zero? '(scheme-number) =zero?-sche)
    (put '=zero? '(rational) =zero?-rat)
    (put '=zero? '(complex) =zero?-com)
    (put '=zero? '(poly) =zero?-poly)
    'done)


(define (=zero? x)
    (apply-generic '=zero? x))
























