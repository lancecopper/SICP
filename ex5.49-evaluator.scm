(load "/home/lancecopper/code/SICP/compiler.scm")

;;; 
(define eceval
  (make-machine
   ;'(exp env val proc argl continue unev)
   eceval-operations
  '(start-point
      (branch (label external-entry))      ; branches if flag is set
    read-eval-print-loop
      (perform (op initialize-stack))
      (perform
       (op prompt-for-input) (const ";;; EC-Eval input:"))
      (assign exp (op read))
      ;; modified
      (perform (op compile-and-run) (reg exp))
      (goto (label start-point))
      ;; 
    external-entry
      (perform (op initialize-stack))
      (assign env (op get-global-environment))
      (assign continue (label print-result))
      (goto (reg val))
    print-result
      (perform (op user-print) (reg val))
      (goto (label read-eval-print-loop))
      )))



(define (user-print object)
  (cond ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        ((compiled-procedure? object)
         (display '<compiled-procedure>))
        (else (display object))))

(define (compile-and-go expression)
  (let ((instructions
         (assemble (statements
                    (compile expression 'val 'return))
                   eceval)))
    (set! the-global-environment (setup-environment))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flag true)
    (start eceval)))















