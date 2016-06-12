(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;;; test
(define x (cons 'a 'b))
(define y (cons 'c 'd))
(define z (cons x y))

(count-pairs z)
3

;;;
(define x (cons 'a 'b))
(define y (cons x 'd))
(define z (cons x y))

(count-pairs z)
4

;;;
(define x (cons 'a 'b))
(define y (cons x x))
(define z (cons y y))

(count-pairs z)
7

;;;

(define x (list 'a 'b 'c ))

(set-cdr! (cddr x) x)

(count-pairs x)
