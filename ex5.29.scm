(start eceval)

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))



;;; total-pushes
s(n)=s(n-1)+s(n-2)+40   (n>=2)
s(n)=a * fib(n+1) + b     (a=46, b=-40)
s(n)=56*fib(n+1)-40

;;; maximum-depth
5n+3

;;; detail
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)
(fib 8)
(fib 9)
(fib 10)


(total-pushes = 16 maximum-depth = 8)
;;; EC-Eval value:
1

;;; EC-Eval input:

(total-pushes = 72 maximum-depth = 13)
;;; EC-Eval value:
1

;;; EC-Eval input:

(total-pushes = 128 maximum-depth = 18)
;;; EC-Eval value:
2

;;; EC-Eval input:

(total-pushes = 240 maximum-depth = 23)
;;; EC-Eval value:
3

;;; EC-Eval input:

(total-pushes = 408 maximum-depth = 28)
;;; EC-Eval value:
5

;;; EC-Eval input:

(total-pushes = 688 maximum-depth = 33)
;;; EC-Eval value:
8

;;; EC-Eval input:

(total-pushes = 1136 maximum-depth = 38)
;;; EC-Eval value:
13

;;; EC-Eval input:

(total-pushes = 1864 maximum-depth = 43)
;;; EC-Eval value:
21

;;; EC-Eval input:

(total-pushes = 3040 maximum-depth = 48)
;;; EC-Eval value:
34

;;; EC-Eval input:

(total-pushes = 4944 maximum-depth = 53)
;;; EC-Eval value:
55

;;; EC-Eval input:






