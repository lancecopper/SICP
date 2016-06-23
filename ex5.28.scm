;;; test
(start eceval)

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;;;
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

;;; total-pushes
37n+40(ori:35n+34)

;;; maximumu-depth
3n+17(ori:10)



(total-pushes = 77 maximum-depth = 20)
;;; EC-Eval value:
1

;;; EC-Eval input:

(total-pushes = 114 maximum-depth = 23)
;;; EC-Eval value:
2

;;; EC-Eval input:

(total-pushes = 151 maximum-depth = 26)
;;; EC-Eval value:
6

;;; EC-Eval input:

(total-pushes = 188 maximum-depth = 29)
;;; EC-Eval value:
24

;;; EC-Eval input:

(total-pushes = 225 maximum-depth = 32)
;;; EC-Eval value:
120

;;; EC-Eval input:

(total-pushes = 262 maximum-depth = 35)
;;; EC-Eval value:
720

;;; EC-Eval input:

(total-pushes = 299 maximum-depth = 38)
;;; EC-Eval value:
5040

;;; EC-Eval input:

(total-pushes = 336 maximum-depth = 41)
;;; EC-Eval value:
40320

;;; EC-Eval input:

(total-pushes = 373 maximum-depth = 44)
;;; EC-Eval value:
362880


;;; test

(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))



;;; total-pushes
34n-16(ori:32n-16)

;;; maximumu-depth
8n+3(ori:5n+3)



(total-pushes = 18 maximum-depth = 11)
;;; EC-Eval value:
1

;;; EC-Eval input:

(total-pushes = 52 maximum-depth = 19)
;;; EC-Eval value:
2

;;; EC-Eval input:

(total-pushes = 86 maximum-depth = 27)
;;; EC-Eval value:
6

;;; EC-Eval input:

(total-pushes = 120 maximum-depth = 35)
;;; EC-Eval value:
24

;;; EC-Eval input:

(total-pushes = 154 maximum-depth = 43)
;;; EC-Eval value:
120

;;; EC-Eval input:

(total-pushes = 188 maximum-depth = 51)
;;; EC-Eval value:
720

;;; EC-Eval input:

(total-pushes = 222 maximum-depth = 59)
;;; EC-Eval value:
5040

;;; EC-Eval input:

(total-pushes = 256 maximum-depth = 67)
;;; EC-Eval value:
40320

;;; EC-Eval input:

(total-pushes = 290 maximum-depth = 75)
;;; EC-Eval value:
362880


