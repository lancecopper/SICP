
;;; machine
(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)    
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    ((machine 'install-labels)
        (map 
            (lambda (label) (list (cdadr label) (car label))) 
            (machine 'labels)))
    machine))

;;; register
(define (make-register name)
  (let ((contents '*unassigned*)
        (tracing #f))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
                (if tracing
                    (begin
                        (newline)
                        (display (list "{ register name:" name "}  "
                                       "{ old contents:"  contents "}  "
                                       "{ new contents:" value "}"))))
                (set! contents value)))
            ((eq? message 'trace-on)
                (set! tracing #t))
            ((eq? message 'trace-off)
                (set! tracing #f))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))


(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

;;; stack
(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))    
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes  '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else
             (error "Unknown request -- STACK" message))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

;;; make-new-machine

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (instruction-trace #f)
        (instruction-counting 0)
        (labels '())
        (breakpoint-list (make-breakpoint-table))
        (present-label '*no-label*)
        (present-label-offset 0)
        (proceed-point '*unassigned*))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))
                 (list 'trace-on
                       (lambda () (set! instruction-trace #t)))
                 (list 'trace-off
                       (lambda () (set! instruction-trace #f)))))
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
              (begin
                (allocate-register name)
                (lookup-register name)))))
      ;;; uncompleted
      (define (lookup-breakpoint present-label present-label-offset)
        ((breakpoint-list 'lookup-breakpoint) present-label present-label-offset))
      (define (set-breakpoint! label n)
        ((breakpoint-list 'set-breakpoint!) label n))
      (define (cancel-breakpoint! label n)
        ((breakpoint-list 'cancel-breakpoint!) label n))
      (define (cancel-all-breakpoints!)
        ((breakpoint-list 'cancel-all-breakpoints!)))
      (define (proceed)
        (proceed-point))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              (begin
                (set! present-label '*no-label*)
                (set! present-label-offset 0)
                'done)
              (let* ((inst (car insts))
                     (inst-text (instruction-text inst))
                     (inst-proc (instruction-execution-proc inst)))
                  (set! instruction-counting (+ 1 instruction-counting))
                  (set! present-label-offset (+ 1 present-label-offset))
                  (let* ((find-label (assoc inst-proc labels))
                         (proceed (lambda () 
                           (if instruction-trace
                              (begin
                                (if find-label
                                    (begin
                                        (newline)
                                        (display "label:  @@@")
                                        (display (cadr find-label))
                                        (display "@@@")))
                                (newline)
                                (display "$$$performing instruction:")
                                (newline)
                                (display (instruction-text (car insts)))))
                           ((instruction-execution-proc (car insts)))
                           (execute))))
                    (if find-label
                        (begin
                          (set! present-label (cadr find-label))
                          (set! present-label-offset 1)))
                    (if (lookup-breakpoint present-label present-label-offset)
                        (begin
                            (set! proceed-point proceed)
                            (newline)
                            (display (list "{ breakpoint:" "label =" present-label
                                           "offset =" present-label-offset)))
                        (proceed)))))))
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
              ((eq? message 'instruction-counting) instruction-counting)
              ((eq? message 'reset-instruction-counting) 
                (set! instruction-counting 0))
              ((eq? message 'labels) labels)
              ((eq? message 'install-labels)
                (lambda (label-val) (set! labels label-val)))
              ((eq? message 'trace-on-register)
                (lambda (reg-name) ((lookup-register reg-name) 'trace-on)))
              ((eq? message 'trace-off-register)
                (lambda (reg-name) ((lookup-register reg-name) 'trace-off)))
              ((eq? message 'register-table) register-table)
              ((eq? message 'lookup-breakpoint) lookup-breakpoint)
              ((eq? message 'set-breakpoint!) set-breakpoint!)
              ((eq? message 'cancel-breakpoint!) cancel-breakpoint!)
              ((eq? message 'cancel-all-breakpoints!) cancel-all-breakpoints!)
              ((eq? message 'proceed) proceed)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

;;; convenient interface
(define (start machine)
  (machine 'start))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

;;; assembler
(define (assemble controller-text machine)
  (extract-labels controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      insts)))
    
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (receive insts
                        (cons (make-label-entry next-inst
                                                insts)
                              labels))
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels)))))))


(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    ((machine 'install-labels) labels)
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc! 
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts)))


(define (make-instruction text)
  (cons text '()))
(define (instruction-text inst)
  (car inst))
(define (instruction-execution-proc inst)
  (cdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))


(define (make-label-entry label-name insts)
  (cons label-name insts))
(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))

;;; make-execution-procedure

(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))

;;; assign instructions
;;; (assign reg-name value-exp)
;;;
(define (make-assign inst machine labels operations pc)
  (let ((target
         (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()                ; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

;;; Test, branch, and goto instructions
(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))
(define (test-condition test-instruction)
  (cdr test-instruction))


(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))
(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register machine
                                (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE"
                       inst)))))
(define (goto-dest goto-instruction)
  (cadr goto-instruction))

;;; Other instructions

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))
(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))    
      (advance-pc pc))))
(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))
(define (perform-action inst) (cdr inst))

;;; Execution procedures for subexpressions
(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label labels
                              (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else
         (error "Unknown expression type -- ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))


(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                  (make-primitive-exp e machine labels))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE" symbol))))

;;; breakpoint-table

(define (make-breakpoint-table)
  (let ((local-table (list '*table*)))
    (define (lookup-breakpoint label offset)
      (let ((subtable (assoc label (cdr local-table))))
        (if subtable
            (let ((record (assoc offset (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (set-breakpoint! label offset)
      (let ((subtable (assoc label (cdr local-table))))
        (if subtable
            (let ((record (assoc offset (cdr subtable))))
              (if record
                  (set-cdr! record #t)
                  (set-cdr! subtable
                            (cons (cons offset #t)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list label
                                  (cons offset #t))
                            (cdr local-table)))))
      'ok)
    (define (cancel-breakpoint! label offset)
      (let ((subtable (assoc label (cdr local-table))))
        (if subtable
            (let ((record (assoc offset (cdr subtable))))
              (if record
                  (set-cdr! record #f)
                  (set-cdr! subtable
                            (cons (cons offset #f)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list label
                                  (cons offset #f))
                            (cdr local-table)))))
      'ok)
    (define (cancel-all-breakpoints!)
        (set! local-table (list '*table*)))
    (define (dispatch m)
      (cond ((eq? m 'lookup-breakpoint) lookup-breakpoint)
            ((eq? m 'set-breakpoint!) set-breakpoint!)
            ((eq? m 'cancel-breakpoint!) cancel-breakpoint!)
            ((eq? m 'cancel-all-breakpoints!) cancel-all-breakpoints!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (set-breakpoint! machine label n)
    ((machine 'set-breakpoint!) label n))


(define (proceed-machine machine)
    (machine 'proceed))


(define (cancel-breakpoint! machine label n)
    ((machine 'cancel-breakpoint!) label n))


(define (cancel-all-breakpoints! machine)
    (machine 'cancel-all-breakpoints!))




;;;
(define (iter-display dis-list)
   (if (null? dis-list)
       'done
       (begin
         (newline)
         (display (car dis-list))
         (iter-display (cdr dis-list)))))





