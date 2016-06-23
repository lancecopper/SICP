(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (compound-branch (make-label 'compound-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))
          (test (op compiled-procedure? (reg proc)))
          (branch (label ,compiled-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compound-branch
         (compound-proc-appl target compiled-linkage))
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage linkage
          (make-instruction-sequence '(proc argl)
                                     (list target)
           `((assign ,target
                     (op apply-primitive-procedure)
                     (reg proc)
                     (reg argl)))))))
       after-call))))

(define (compound-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
           `((perform (op display) (const 'into-compound))
             (assign continue (label ,linkage))
             (goto (reg comapp)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return))) 
             (make-instruction-sequence '(proc) all-regs 
              `((assign continue (label ,proc-return)) 
                (goto (reg compapp)) 
                ,proc-return 
                (assign ,target (reg val)) 
                (goto (label ,linkage)))))) 
        ((and (eq? target 'val) (eq? linkage 'return)) 
         (make-instruction-sequence '(proc continue) all-regs 
          '((goto (reg compapp)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE"
                target))))

;;; test

(load "/home/lancecopper/code/SICP/5.57.scm")

;;; 


(compile-and-go
 '(define (f x) (g x)))

(define (g y)
  (* y (+ y 1)))




(define foo
(compile 
  '(define (f x) (* (g x) 3))
  'val 
  'return))

(iter-display (caddr foo))

;;; 

  (assign val (op make-compiled-procedure) (label entry23) (reg env))
  (goto (label after-lambda22))
entry23
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (x)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (save continue)
  (save proc)
  (assign val (const 3))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const g) (reg env))
  (assign val (op lookup-variable-value) (const x) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch27))
  (test (op compound-procedure?) (reg proc))
  (branch (label compound-branch25))
compiled-branch26
  (assign continue (label after-call24))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
compound-branch25
  (perform (op display) (const (quote into-compound1)))
  (assign continue (label after-call24))
  (perform (op display) (const (quote into-compound1-before-goto)))
  (goto (reg comapp))
primitive-branch27
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call24
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch31))
  (test (op compound-procedure?) (reg proc))
  (branch (label compound-branch29))
compiled-branch30
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
compound-branch29
  (perform (op display) (const (quote into-compound3)))
  (goto (reg compapp))
primitive-branch31
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call28
after-lambda22
  (perform (op define-variable!) (const f) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))
compound-apply
      (perform (op display) (const into-compound-apply))
      (assign unev (op procedure-parameters) (reg proc))
      (assign env (op procedure-environment) (reg proc))
      (assign env (op extend-environment)
                  (reg unev) (reg argl) (reg env))
      (assign unev (op procedure-body) (reg proc))
      (goto (label ev-sequence))


;;; test
(get-register-contents eceval 'compapp)
