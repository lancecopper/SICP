
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


(define (num-tower '(integer rational real complex)))
(define tower num-tower)

(define (higher-in-tower x y tower)
  (cond ((null? tower) (error "cannot find in tower" (list x y)))
        ((= (car tower) (type-tag x)) y)
        ((= (car tower) (type-tag y)) x)
        (else (higher-in-tower x y (cdr tower)))))

(define (lower-in-tower x y tower)
  (cond ((null? tower) (error "cannot find in tower" (list x y)))
        ((= (car tower) (type-tag x)) x)
        ((= (car tower) (type-tag y)) y)
        (else (lower-in-tower x y (cdr tower)))))

(define (raise-until-equal x y)
  (if (= (type-tag x) (type-tag y))
      x
      (raise-until-equal (raise x) y)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                  (error  "No method for these types" (list op type-tags))
                    (cond ((= (higher-in-tower a1 a2 tower) a1)
                           (apply-generic op a1 (raise-until-equal a2 a1)))
                          ((= (higher-in-tower a1 a2 tower) a2)
                           (apply-generic op (raise-until-equal a1 a2) a2))
                          (else
                           (error "No method for these types"
                                  (list op type-tags)))))))
              (error "No method for these types"
                     (list op type-tags)))))))

  
