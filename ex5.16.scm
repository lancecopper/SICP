
(load "/home/lancecopper/code/SICP/machine-model.scm")

(define fact-machine
  (make-machine
   (list (list '= =) 
         (list '- -) 
         (list '* *) 
         (list 'read read)
         (list 'display-prompt 
            (lambda () (newline) (display "input n for fact(n)"))))
   '(controller
       (perform (op trace-on))
     loop-start
       (perform (op display-prompt))
       (perform (op initialize-stack))
       (assign n (op read))
       (assign continue (label fact-done))     ; set up final return address
     fact-loop
       (test (op =) (reg n) (const 1))
       (branch (label base-case))
       ;; Set up for the recursive call by saving n and continue.
       ;; Set up continue so that the computation will continue
       ;; at after-fact when the subroutine returns.
       (save continue)
       (save n)
       (assign n (op -) (reg n) (const 1))
       (assign continue (label after-fact))
       (goto (label fact-loop))
     after-fact
       (restore n)
       (restore continue)
       (assign val (op *) (reg n) (reg val))   ; val now contains n(n - 1)!
       (goto (reg continue))                   ; return to caller
     base-case
       (assign val (const 1))                  ; base case: 1! = 1
       (goto (reg continue))                   ; return to caller
     fact-done
       (perform (op print-stack-statistics)) 
       (goto (label loop-start)))
   ))


;;; test
(start fact-machine)



