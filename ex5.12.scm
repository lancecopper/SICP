;;; test
(load "/home/lancecopper/code/SICP/machine-model5.12.scm")

(define fib-machine
  (make-machine
   '(n val continue)
   (list (list '< <) (list '- -) (list '+ +))
   '(controller
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
       (restore continue)
       ;; set up to compute Fib(n - 2)
       (assign n (op -) (reg n) (const 2))
       (save continue)
       (assign continue (label afterfib-n-2))
       (save val)                         ; save Fib(n - 1)
       (goto (label fib-loop))
     afterfib-n-2                         ; upon return, val contains Fib(n - 2)
       (assign n (reg val))               ; n now contains Fib(n - 2)
       (restore val)                      ; val now contains Fib(n - 1)
       (restore continue)
       (assign val                        ;  Fib(n - 1) +  Fib(n - 2)
               (op +) (reg val) (reg n)) 
       (goto (reg continue))              ; return to caller, answer is in val
     immediate-answer
       (assign val (reg n))               ; base case:  Fib(n) = n
       (goto (reg continue))
     fib-done)))


(set-register-contents! fib-machine 'n  7)

(start fib-machine)

(get-register-contents fib-machine 'val)

; a
(iter-display (fib-machine 'sorted-instruction-list))
; b
(iter-display (fib-machine 'entry-register-list))
; c
(iter-display (fib-machine 'saved-or-restored-register-list))
; d
(iter-display (fib-machine 'register-assigned-source-table))

;;; answer
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (sorted-instruction-list '())
        (entry-register-list '())
        (saved-or-restored-register-list '())
        (register-assigned-source-table '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (upgrade-entry-register-list reg)
        (define (insert! reg entry-register-list)
            (if (null? entry-register-list)
                (cons reg entry-register-list)
                (let ((reg-name (cadr reg))
                      (reg-car-name (cadr (car entry-register-list))))
                    (if (equal? reg (car entry-register-list))
                        entry-register-list
                        (if (greater-than? reg-car-name reg-name)
                            (cons reg entry-register-list)
                            (cons (car entry-register-list)
                                  (insert! reg (cdr entry-register-list))))))))
        (set! entry-register-list (insert! reg entry-register-list)))
      (define (upgrade-saved-or-restored-register-list reg-name)
        (define (insert! reg-name saved-or-restored-register-list)
            (if (null? saved-or-restored-register-list)
                (cons (list 'reg reg-name) saved-or-restored-register-list)
                (let ((reg (list 'reg reg-name))
                      (reg-car-name (cadr (car saved-or-restored-register-list))))
                    (if (equal? reg (car saved-or-restored-register-list))
                        saved-or-restored-register-list
                        (if (greater-than? reg-car-name reg-name)
                            (cons reg saved-or-restored-register-list)
                            (cons (car saved-or-restored-register-list)
                                  (insert! reg-name (cdr saved-or-restored-register-list))))))))
        (set! saved-or-restored-register-list 
            (insert! reg-name saved-or-restored-register-list)))
      (define (upgrade-register-assigned-source-table reg-name source)
        (let ((reg-source-list 
              (assoc reg-name register-assigned-source-table)))
            (define (insert! source source-list)
                (define (is-in? source source-list)
                    (if (null? source-list)
                        #f
                        (if (equal? source (car source-list))
                            #t
                            (is-in? source (cdr source-list)))))
                (if (is-in? source source-list)
                    source-list
                    (cons source source-list)))
            (if reg-source-list
                (let ((cdr-reg-source-list (cdr reg-source-list)))
                    (set-cdr! reg-source-list 
                        (insert! source cdr-reg-source-list)))
                (begin
                    (set! register-assigned-source-table
                        (cons (cons reg-name '()) 
                            register-assigned-source-table))
                    (upgrade-register-assigned-source-table reg-name source)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ;;; this interphace is just for test!
              ((eq? message 'the-instruction-sequence) the-instruction-sequence)
              ;;; newly added interface
              ((eq? message 'sorted-instruction-list) sorted-instruction-list)
              ((eq? message 'install-sorted-instruction-list)
                (lambda (seq) (set! sorted-instruction-list seq)))
              ((eq? message 'entry-register-list) entry-register-list)
              ((eq? message 'upgrade-entry-register-list)
                upgrade-entry-register-list)
              ((eq? message 'saved-or-restored-register-list) 
                saved-or-restored-register-list)
              ((eq? message 'upgrade-saved-or-restored-register-list)
                upgrade-saved-or-restored-register-list)
              ((eq? message 'register-assigned-source-table) 
                register-assigned-source-table)
              ((eq? message 'upgrade-register-assigned-source-table)
                upgrade-register-assigned-source-table)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

;;; a
(define (assemble controller-text machine)
    (let ((instruction-sequence
          (extract-labels controller-text
            (lambda (insts labels)
              (update-insts! insts labels machine)
              insts))))
        ((machine 'install-sorted-instruction-list)       ;;; a
            (sort-instruction instruction-sequence))

        instruction-sequence))


(define (greater-than? text1 text2)
             (string>? (symbol->string text1) (symbol->string text2)))

(define (sort-instruction instruction-sequence)
    (let ((sorted-instruction-list '()))
         (define (insert! instruction sorted-instruction-list)
             (if (null? sorted-instruction-list)
                 (cons instruction sorted-instruction-list)
                 (let ((text-ins-car (instruction-text (car sorted-instruction-list)))
                       (text-ins (instruction-text instruction)))
                     (if (equal? text-ins-car text-ins)
                         sorted-instruction-list
                         (if (greater-than? (car text-ins-car) (car text-ins))
                             (cons instruction sorted-instruction-list)
                             (cons (car sorted-instruction-list)
                                   (insert! instruction 
                                           (cdr sorted-instruction-list))))))))
         (define (iter instruction-sequence sorted-instruction-list)
             (if (null? instruction-sequence)
                 sorted-instruction-list
                 (iter (cdr instruction-sequence)
                       (insert! (car instruction-sequence)
                               sorted-instruction-list))))
         (iter instruction-sequence sorted-instruction-list)))



;;; b

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           ((machine 'upgrade-entry-register-list) dest)
           (let ((reg
                  (get-register machine
                                (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE"
                       inst)))))
(define (goto-dest goto-instruction)
  (cadr goto-instruction))


;;; c

(define (make-save inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name)))
    ((machine 'upgrade-saved-or-restored-register-list)
        reg-name)
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name)))
    ((machine 'upgrade-saved-or-restored-register-list)
        reg-name)
    (lambda ()
      (set-contents! reg (pop stack))    
      (advance-pc pc))))


;;; d (reg-name, source)

(define (make-assign inst machine labels operations pc)
  (let* ((reg-name (assign-reg-name inst))
         (target (get-register machine reg-name))
         (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      ((machine 'upgrade-register-assigned-source-table)
        reg-name value-exp)
      (lambda ()                ; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))



















