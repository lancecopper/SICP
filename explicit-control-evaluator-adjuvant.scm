
(define pri-apply apply)
(define pri-eval eval)

(load "/home/lancecopper/code/SICP/old-env-primitive.scm")
(load "/home/lancecopper/code/SICP/old-eval-apply.scm")
(load "/home/lancecopper/code/SICP/machine-model5.19.scm")

(define apply pri-apply)
(define eval pri-eval)

(define the-global-environment (setup-environment))
(define (get-global-environment)
  the-global-environment)

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? '*unassigned* (car vals))
                '**variable-unbound**
                (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        '**variable-unbound**
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (adjoin-arg val args)
    (append args (list val)))

(define (no-more-exps? exp)
    (null? exp))

(define debug-mode? #f)

(define safe-primitives 
   (list car cdr /)) 

(define (apply-primitive-procedure proc args) 
  (if debug-mode?
      (begin
          (newline)
          (display (list 'apply-primitive proc args)) 
          (newline))) 
  (let ((primitive (primitive-implementation proc))) 
    (if (member primitive safe-primitives) 
        (safe-apply primitive args)     ; ex 5.30 b 
        (apply-in-underlying-scheme 
         primitive args))))
 
(define (safe-apply proc args)          ; ex 5.30 b 
  (if debug-mode?
      (begin
        (newline)
        (display 'safe-apply)
        (newline))) 
  (cond ((or (eq? proc car) 
             (eq? proc cdr)) 
         (safe-car-cdr proc args)) 
        ((eq? proc /) 
         (safe-division proc args)) 
        (else 
         (list 'primitive-error proc args)))) 
 
(define (primitive-error? val)          ; ex 5.30 b 
  (tagged-list? val 'primitive-error)) 
 
(define (safe-car-cdr proc args)        ; ex 5.30 b 
  (if debug-mode?
      (begin
        (newline)
        (display (list 'safe-car-cdr args)) 
        (newline))) 
  (if (not (pair? (car args)))          ; args is a list (args '()) 
      (list 'primitive-error 'arg-not-pair) 
      (apply-in-underlying-scheme proc args))) 
(define (safe-division proc args)       ; ex 5.30 b 
  (if (= 0 (cadr args)) 
      (cons 'primitive-error 'division-by-zero) 
      (apply-in-underlying-scheme proc args)))

;;; safe-check
(define safe-primitives 
   (list car cdr /)) 

(define (apply-primitive-procedure proc args) 
  (if debug-mode?
      (begin
        (newline)
        (display (list 'apply-primitive proc args)) 
        (newline))) 
  (let ((primitive (primitive-implementation proc))) 
    (if (member primitive safe-primitives) 
        (safe-apply primitive args)     ; ex 5.30 b 
        (apply-in-underlying-scheme 
         primitive args)))) 
 
(define (safe-apply proc args)          ; ex 5.30 b 
  (if debug-mode?
      (begin
        (newline) 
        (display 'safe-apply) 
        (newline))) 
  (cond ((or (eq? proc car) 
             (eq? proc cdr)) 
         (safe-car-cdr proc args)) 
        ((eq? proc /) 
         (safe-division proc args)) 
        (else 
         (list 'primitive-error proc args)))) 
 
(define (primitive-error? val)          ; ex 5.30 b 
  (tagged-list? val 'primitive-error)) 
 
(define (safe-car-cdr proc args)        ; ex 5.30 b 
  (if debug-mode?
      (begin
        (newline)
        (display (list 'safe-car-cdr args)) 
        (newline))) 
  (if (not (pair? (car args)))          ; args is a list (args '()) 
      (list 'primitive-error 'arg-not-pair) 
      (apply-in-underlying-scheme proc args))) 
(define (safe-division proc args)       ; ex 5.30 b 
  (if (= 0 (cadr args)) 
      (list 'primitive-error 'division-by-zero) 
      (apply-in-underlying-scheme proc args)))

(define (prim-compile-and-run expression) 
 (assemble (statements 
            (compile expression 'val 'return)) 
           eceval))

(define (compile-and-run? exp) 
 (tagged-list? exp 'compile-and-run))

(define (compile-and-run-exp exp) 
   (cadadr exp))


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



(define eceval-operations
    (list
        (list 'prompt-for-input prompt-for-input)
        (list 'read read)
        (list 'get-global-environment get-global-environment)
        (list 'announce-output announce-output)
        (list 'user-print user-print)
        (list 'self-evaluating? self-evaluating?)
        (list 'variable? variable?)
        (list 'quoted? quoted?)
        (list 'assignment? assignment?)
        (list 'definition? definition?)
        (list 'if? if?)
        (list 'lambda? lambda?)
        (list 'begin? begin?)
        (list 'cond? cond?)
        (list 'let? let?)
        (list 'application? application?)
        (list 'lookup-variable-value lookup-variable-value)
        (list 'text-of-quotation text-of-quotation)
        (list 'lambda-parameters lambda-parameters)
        (list 'lambda-body lambda-body)
        (list 'make-procedure make-procedure)
        (list 'operands operands)
        (list 'operator operator)
        (list 'empty-arglist (lambda () '()))
        (list 'no-operands? no-operands?)
        (list 'first-operand first-operand)
        (list 'last-operand? last-exp?)
        (list 'rest-operands rest-operands)
        (list 'adjoin-arg adjoin-arg)
        (list 'primitive-procedure? primitive-procedure?)
        (list 'compound-procedure? compound-procedure?)
        (list 'apply-primitive-procedure apply-primitive-procedure)
        (list 'procedure-parameters procedure-parameters)
        (list 'procedure-environment procedure-environment)
        (list 'extend-environment extend-environment)
        (list 'procedure-body procedure-body)
        (list 'begin-actions begin-actions)
        (list 'first-exp first-exp)
        (list 'last-exp? last-exp?)
        (list 'rest-exps rest-exps)
        (list 'cond->if cond->if)
        (list 'let->combination let->combination)
        (list 'if-predicate if-predicate)
        (list 'true? true?)
        (list 'if-alternative if-alternative)
        (list 'if-consequent if-consequent)
        (list 'assignment-variable assignment-variable) 
        (list 'assignment-value assignment-value)
        (list 'set-variable-value! set-variable-value!)
        (list 'definition-variable definition-variable)
        (list 'definition-value definition-value)
        (list 'define-variable! define-variable!)
        (list 'no-more-exps? no-more-exps?)
        (list 'equal? equal?)
        (list 'apply-primitive-procedure apply-primitive-procedure)
        (list 'primitive-error? primitive-error?)
        (list 'list list)
        (list 'false? false?)
        (list 'cons cons)
        (list 'display display)
        (list 'newline newline)
        (list 'prim-compile-and-run prim-compile-and-run)
        (list 'compile-and-run? compile-and-run?)
        (list 'compile-and-run-exp compile-and-run-exp)
        (list 'compile-and-run compile-and-run)
        ))
