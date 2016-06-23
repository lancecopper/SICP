
;;; a
(controller
   (assign continue (label fib-done))
 fib-loop
   (test (op <) (reg n) (const 2))
   (branch (label immediate-answer))
   ;; set up to compute Fib(n - 1)
   (save continue)
   (assign continue (label afterfib-n-1))
   (save n)                           ; save old value of n
   (assign n (op -) (reg n) (const 1)); clobber n to n - 1
   (goto (label fib-loop))            ; perform recursive call
 afterfib-n-1                         ; upon return, val contains Fib(n - 1)
   (restore n)
   ;; set up to compute Fib(n - 2)
   (assign n (op -) (reg n) (const 2))
   (assign continue (label afterfib-n-2))
   (save val)                         ; save Fib(n - 1)
   (goto (label fib-loop))
 afterfib-n-2                         ; upon return, val contains Fib(n - 2)
   (restore n)
   (restore continue)
   (assign val                        ;  Fib(n - 1) +  Fib(n - 2)
           (op +) (reg val) (reg n)) 
   (goto (reg continue))              ; return to caller, answer is in val
 immediate-answer
   (assign val (reg n))               ; base case:  Fib(n) = n
   (goto (reg continue))
 fib-done)

;;; b
(define (make-save inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name)))
    (lambda ()
      (push stack (cons reg-name (get-contents reg)))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
    (let* ((reg-name (stack-inst-reg-name inst))
           (reg (get-register machine reg-name))
           (stack-content (pop stack)))
        (if (eq? reg-name (car stack-content))
            (lambda ()
              (set-contents! reg (pop stack))    
              (advance-pc pc))
            (error 
                "invalid register name for stack pop--MAKE-RESTORE" 
                reg-name))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

;;; c

(define (make-new-machine)
  (let
    ... ...
      
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (begin
                (set! register-table
                      (cons (list name (make-register name))
                            register-table))
                (set! register-stack-table              ;;; changed
                      (cons (list name (make-stack))    ;;;
                            register-stack-table))))    ;;;
        'register-allocated)
      ... ...

      (define (dispatch message)
        (cond ... ...

              ((eq? message 'stack) register-stack-table)       ;;; changed
              
              ... ...
      dispatch)))

(define (lookup-register-stack register-name register-stack-table)
        (let ((val (assoc name register-stack-table)))
          (if val
              (cadr val)
              (error "Unknown register-stack:" name))))

(define (make-save inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name))
         (stack (lookup-register-stack reg-name register-stack-table)))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg-name (stack-inst-reg-name inst))
        (reg (get-register machine reg-name))
        (stack (lookup-register-stack reg-name register-stack-table)))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))
















