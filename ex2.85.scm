;;; project

(define (install-project)
    (define (project-rat x)
        (make-scheme-number (numer x)))
    (define (real2rat-denom x)
        (define (iter x result)
            (if (integer? x)
                result
                (iter (* 10 x) (* 10 result))))
        (iter x 1))
    (define (project-real x)
        (if (integer? x)
            (make-rat x 1)
            (make-rat (* (real2rat-denom x) x) (real2rat-denom x))))
    (define (project-com x)
        (make-real (real-part x)))
    (put 'project '(rational) project-rat)
    (put 'project '(real) project-real)
    (put 'project '(complex) project-com)
    'done)


(define (project x)
    (apply-generic 'project x))


;;;Then a number can be dropped if, when we project it 
;;; and raise the result back to the type we started with, 
;;; we end up with something equal to what we started with.



;;; auxiliary function
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



;;; drop

(define (num-tower '(integer rational real complex)))
(define tower num-tower)

(define (drop x)
    (let ((project-x (project x)))
         (if (equ? (raise (project-x)) x)
             (drop project-x)
             x)))


;;; apply-generic
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (let ((res (apply proc (map contents args))))
            (if (or (eq? op 'raise) (eq? op 'equ?)) 
                res 
                (drop res)))
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



