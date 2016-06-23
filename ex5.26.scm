;;; test
(start eceval)

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;;; answer
;;; a
10

;;; b
35n+34

;;; detail
(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)
(factorial 5)
(factorial 6)
(factorial 7)
(factorial 8)
(factorial 9)
(factorial 10)
(factorial 11)

;;; EC-Eval input:
(factorial 1)

(total-pushes = 69 maximum-depth = 10)
;;; EC-Eval value:
1

;;; EC-Eval input:
(factorial 2)

(total-pushes = 104 maximum-depth = 10)
;;; EC-Eval value:
2

;;; EC-Eval input:
(factorial 3)

(total-pushes = 139 maximum-depth = 10)
;;; EC-Eval value:
6

;;; EC-Eval input:
(factorial 4)

(total-pushes = 174 maximum-depth = 10)
;;; EC-Eval value:
24

;;; EC-Eval input:
(factorial 5)

(total-pushes = 209 maximum-depth = 10)
;;; EC-Eval value:
120

;;; EC-Eval input:
(factorial 6)

(total-pushes = 244 maximum-depth = 10)
;;; EC-Eval value:
720

;;; EC-Eval input:
(factorial 7)

(total-pushes = 279 maximum-depth = 10)
;;; EC-Eval value:
5040

;;; EC-Eval input:
(factorial 8)

(total-pushes = 314 maximum-depth = 10)
;;; EC-Eval value:
40320

;;; EC-Eval input:
(factorial 9)

(total-pushes = 349 maximum-depth = 10)
;;; EC-Eval value:
362880

;;; EC-Eval input:
(factorial 10)

(total-pushes = 384 maximum-depth = 10)
;;; EC-Eval value:
3628800

;;; EC-Eval input:
(factorial 11)

(total-pushes = 419 maximum-depth = 10)
;;; EC-Eval value:
39916800

;;; EC-Eval input:


