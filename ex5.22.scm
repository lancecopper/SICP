
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


;;; a
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(load "/home/lancecopper/code/SICP/machine-model5.19.scm")

(define append-machine
  (make-machine
   (list (list 'null? null?)  
         (list 'car car)
         (list 'cdr cdr)
         (list 'cons cons)
         (list 'done (lambda () (newline) (display "done"))))
   '(controller
       (assign continue (label append-done))
    append-loop
       (test (op null?) (reg x))
       (branch (label x-null))
       (assign x-car (op car) (reg x))
       (save continue)
       (save x-car)
       (assign x (op cdr) (reg x))
       (assign continue (label after-append))
       (goto (label append-loop))
    after-append
       (restore x-car)
       (restore continue)
       (assign y (op cons) (reg x-car) (reg y))
       (goto (reg continue))
    x-null
       (goto (reg continue))
    append-done
       (perform (op done)))))

(define x (list 1 2 3))
(define y (list 4 5 6))

(set-register-contents! append-machine 'x x)
(set-register-contents! append-machine 'y y)

(start append-machine)

(get-register-contents append-machine 'y)


;;; b

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)



(define append!-machine
  (make-machine
   (list (list 'set-cdr! set-cdr!)
         (list 'last-pair last-pair)
         (list 'done (lambda () (newline) (display "done"))))
   '(controller
       (assign last-pair (op last-pair) (reg x))
       (perform (op set-cdr!) (reg last-pair) (reg y))
    append!-done
       (perform (op done)))))


(define x (list 1 2 3 5 6 7))
(define y (list 8 9 10))

(set-register-contents! append!-machine 'x x)
(set-register-contents! append!-machine 'y y)

(start append!-machine)

(get-register-contents append!-machine 'x)





