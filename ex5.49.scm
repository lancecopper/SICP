(define (compile-and-run-meta)
  (let ((called #f))
    (define (dispatch expression)
      (let ((instructions
             (assemble (statements
                        (compile expression 'val 'return))
                       eceval)))
        (if called
            (begin 
              (set-register-contents! eceval 'val instructions)
              (set-register-contents! eceval 'flag true))
            (begin
              (set! called #t)
              (set! the-global-environment (setup-environment))
              (set-register-contents! eceval 'val instructions)
              (set-register-contents! eceval 'flag true)
              (start eceval)))))
  dispatch))

(define compile-and-run (compile-and-run-meta))

;;; test

(load "/home/lancecopper/code/SICP/ex5.49-evaluator.scm")

(set-register-contents! eceval 'flag #f)
(start eceval)


(compile-and-run
 '1)

(define (f x) (* (factorial x) (factorial x)))




