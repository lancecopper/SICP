(load "/home/lancecopper/code/SICP/machine-model.scm")

(define gcd-machine
  (make-machine
   '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b
       (test (op =) (reg b) (const 0))
       (branch (label gcd-done))
       (assign t (op rem) (reg a) (reg b))
       (assign a (reg b))
       (assign b (reg t))
       (goto (label test-b))
     gcd-done)))


(set-register-contents! gcd-machine 'a 206)
;done
(set-register-contents! gcd-machine 'b 40)
;done
(start gcd-machine)
;done
(get-register-contents gcd-machine 'a)
;2


;;;

; a
(define expt-rec-machine
  (make-machine
   '(continue n val b)
   (list (list '= =) (list '- -) (list '* *))
   '(controller
       (assign continue (label expt-done))     
     expt-loop
       (test (op =) (reg n) (const 0))
       (branch (label base-case))
       (save continue)
       (assign n (op -) (reg n) (const 1))
       (assign continue (label after-expt))
       (goto (label expt-loop))
     after-expt
       (restore continue)
       (assign val (op *) (reg b) (reg val))
       (goto (reg continue))                   
     base-case
       (assign val (const 1))                  
       (goto (reg continue))                   
     expt-done)))

(set-register-contents! expt-rec-machine 'n 4)
;done
(set-register-contents! expt-rec-machine 'b 3)
;done
(start expt-rec-machine)
;done
(get-register-contents expt-rec-machine 'val)
;81




;;; b
(define expt-iter-machine
  (make-machine
   '(counter product n b)
   (list (list '= =) (list '- -) (list '* *))
   '(controller
        (assign counter (reg n))
        (assign product (const 1))
     
     expt-loop
        (test (op =) (reg counter) (const 0))
        (branch (label expt-done))
        (assign counter (op -) (reg counter) (const 1))
        (assign product (op *) (reg product) (reg b))
        (goto (label expt-loop))

     expt-done)))

(set-register-contents! expt-iter-machine 'n 10)
;done
(set-register-contents! expt-iter-machine 'b 2)
;done
(start expt-iter-machine)
;done
(get-register-contents expt-iter-machine 'product)
;81





