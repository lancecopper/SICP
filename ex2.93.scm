(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))

(define (make-rat n d)
    (let ((g (greatest-common-divisor n d)))
      (cons (/ n g) (/ d g))))


(define (div-terms L1 L2) 
  (if (empty-termlist? L1) 
    (list (the-empty-termlist) (the-empty-termlist)) 
    (let ((t1 (first-term L1)) 
          (t2 (first-term L2))) 
      (if (> (order t2) (order t1)) 
        (list (the-empty-termlist) L1) 
        (let ((new-c (div (coeff t1) (coeff t2))) 
          (new-o (- (order t1) (order t2))) 
          (new-t (make-term new-o new-c))) 
          (let ((rest-of-result 
                  (div-terms (add-terms L1 (negate (mul-terms (list new-t) L2))) 
                              L2))) 
            (list (adjoin-term new-t 
                               (car rest-of-result)) 
                  (cadr rest-of-result))))))))



;;; greatest-common-divisor
(define (greatest-common-divisor a b) 
  (apply-generic 'greatest-common-divisor a b)) 
 
;; add into scheme-number package 
(put 'greatest-common-divisor '(scheme-number scheme-number) 
     (lambda (a b) (gcd a b))) 
 
;; add into polynomial package 
(define (remainder-terms p1 p2) 
  (cadr (div-terms p1 p2))) 
 
(define (gcd-terms a b) 
  (if (empty-termlist? b) 
    a 
    (gcd-terms b (remainder-terms a b)))) 
 
(define (gcd-poly p1 p2) 
  (if (same-varaible? (variable p1) (variable p2)) 
        (make-poly (variable p1) 
                   (gcd-terms (term-list p1) 
                              (term-list p2)) 
        (error "not the same variable -- GCD-POLY" (list p1 p2))))) 
 
(put 'greatest-common-divisor '(polynomial polynomial) 
         (lambda (a b) (tag (gcd-poly a b)))) 




;;; rational number
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (greatest-common-divisor n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define p1 (make-polynomial 'x '((2 1)(0 1))))
(define p2 (make-polynomial 'x '((3 1)(0 1))))
(define rf (make-rational p2 p1))


