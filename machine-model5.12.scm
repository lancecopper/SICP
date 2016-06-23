
;;; machine
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)    
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

;;; register
(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

;;; stack
(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            (else (error "Unknown request -- STACK"
                         message))))
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

;;; Other instructions


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
                (if (label-exp? e)
                    (error "can't operate on label -- MAKE-OPERATION-EXP" e)
                    (make-primitive-exp e machine labels)))
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


;;; test auxiliary function

(define (iter-display dis-list)
   (if (null? dis-list)
       'done
       (begin
         (newline)
         (display (car dis-list))
         (iter-display (cdr dis-list)))))






