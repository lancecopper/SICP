(define apply-in-underlying-scheme apply)
(define eval-in-underlying-scheme eval)

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((and? exp) (eval (and->if exp) env))
        ((or? exp) (eval (or->if exp) env))
        ((let? exp) (eval (let->combination exp) env))
        ((letrec? exp) (eval (letrec->let exp) env))
        ((unless? exp) (eval (unless->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))


(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first (eval (first-operand exps) env)))
           (cons first 
                 (list-of-values (rest-operands exps) env)))))



(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)


;;; self-evalueate, variable, quote, 
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;;; assginment
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))


;;; definition
(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

;;; lambda
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


;;; if
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


;;; begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))


;;; application
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


;;; cond
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))


;;; and
(define (and? expr) (tagged-list? expr 'and))
(define (and-clauses expr) (cdr expr))
(define (and->if expr) 
        (expand-and-clauses (and-clauses expr))) 
(define (expand-and-clauses clauses)      
    (let ((first (car clauses)) 
          (rest (cdr clauses))) 
         (if (null? rest)  
             (make-if first first 'false) 
             (make-if first (expand-and-clauses rest) 'false))))

;;; or
(define (or? expr) (tagged-list? expr 'or))
(define (or-clauses expr) (cdr expr))
(define (or->if expr) 
        (expand-or-clauses (or-clauses expr))) 
(define (expand-or-clauses clauses)
    (let ((first (car clauses)) 
          (rest (cdr clauses)))
         (if (null? rest)
             (make-if first first 'false)
             (make-if first first (expand-or-clauses rest)))))

;;; let
(define (let? expr) (tagged-list? expr 'let)) 
(define (let-vars expr) (map car (cadr expr))) 
(define (let-inits expr) (map cadr (cadr expr))) 
(define (let-body expr) (cddr expr)) 
 
(define (let->combination expr)
    (transform-let (let-vars expr) (let-inits expr) (let-body expr)))

(define (transform-let vars inits body)
    (cons (make-lambda vars body)
          inits))


;;; letrec
(define (letrec? exp) (tagged-list? exp 'letrec))
(define (make-unassigned-letrec vars)
  (if (null? vars)
      '()
      (cons (list (car vars) ''*unassigned*) 
            (make-unassigned-letrec (cdr vars)))))
(define (make-set-letrec vars exps)
  (if (null? vars)
      '()
      (cons (list 'set! (car vars) (car exps))
            (make-set-letrec (cdr vars) (cdr exps)))))
(define (letrec->let exp)
  (let ((lvars (let-vars exp))
        (lexps (let-inits exp)))
        (cons 'let (cons (make-unassigned-letrec lvars)
                         (append (make-set-letrec lvars lexps)
                                 (let-body exp))))))

;;; unless

; ('unless <condition-exp> <usual-value-exp> <exceptional-value-exp>)

(define (unless? exp)
    (tagged-list? exp 'unless))

(define (unless-condition exp)
    (cadr exp))

(define (unless-usual-value exp)
    (caddr exp))

(define (unless-exceptional-value exp)
    (cadddr exp))

(define (unless->if exp)
    (let ((predicate (unless-condition exp))
          (consequent (unless-exceptional-value exp))
          (alternative (unless-usual-value exp)))
         (make-if predicate consequent alternative)))



