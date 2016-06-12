(define (make-from-mag-ang x y)
    (define (dispatch op)
        (cond ((eq? op 'magnitude) x)
              ((eq? op 'angle) y)
              ((eq? op 'real-part) (* x (cos y)))
              ((eq? op 'imag-part) (* x (sin y)))
              (else (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
    dispatch)


(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)


