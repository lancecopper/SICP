(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))




(define (count-pairs x)

    (define (inlist x xlist)
        (if (null? xlist)
            false
            (if (eq? x (car xlist))
                true
                (inlist x (cdr xlist)))))

    (define (iter pairs-counted x)
        (if (not (pair? x)) 
            0
            (if (inlist x pairs-counted)
                0
                (begin
                    (append! pairs-counted (cons x '()))
                    (+ (iter pairs-counted (car x))
                       (iter pairs-counted (cdr x))
                       1)))))
    (iter (list 1) x))


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
