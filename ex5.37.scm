(load "/home/lancecopper/code/SICP/old-env-primitive.scm")
(load "/home/lancecopper/code/SICP/old-eval-apply.scm")
(load "/home/lancecopper/code/SICP/compiler5.37.scm")

(define foo
    (compile
     '(define (factorial n)
        (if (= n 1)
            1
            (* (factorial (- n 1)) n)))
     'val
     'next))


(iter-display (caddr foo))






