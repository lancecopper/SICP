;;; test
(load "/home/lancecopper/code/SICP/5.57.scm")

(set-register-contents! eceval 'flag #f)
(start eceval)

;;; EC-Eval input:
(compile-and-run
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n))))
;;; EC-Eval value:
ok
;;; EC-Eval input:
(factorial 5)
;;; EC-Eval value:
120

(get-register-contents eceval 'val)