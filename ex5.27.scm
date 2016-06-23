
;;; test
(start eceval)

(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

;;; maximum-depth
5n+3

;;; total-pushes
32n-16

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



(total-pushes = 16 maximum-depth = 8)
;;; EC-Eval value:
1

;;; EC-Eval input:

(total-pushes = 48 maximum-depth = 13)
;;; EC-Eval value:
2

;;; EC-Eval input:

(total-pushes = 80 maximum-depth = 18)
;;; EC-Eval value:
6

;;; EC-Eval input:

(total-pushes = 112 maximum-depth = 23)
;;; EC-Eval value:
24

;;; EC-Eval input:

(total-pushes = 144 maximum-depth = 28)
;;; EC-Eval value:
120

;;; EC-Eval input:

(total-pushes = 176 maximum-depth = 33)
;;; EC-Eval value:
720

;;; EC-Eval input:

(total-pushes = 208 maximum-depth = 38)
;;; EC-Eval value:
5040

;;; EC-Eval input:

(total-pushes = 240 maximum-depth = 43)
;;; EC-Eval value:
40320

;;; EC-Eval input:

(total-pushes = 272 maximum-depth = 48)
;;; EC-Eval value:
362880

;;; EC-Eval input:

(total-pushes = 304 maximum-depth = 53)
;;; EC-Eval value:
3628800

;;; EC-Eval input:










