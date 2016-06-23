

(define (find-variable var ct-env)
    (let ((addr-frame 0)
          (addr-offset 0))
        (define (loop-env ct-env)
            (define (scan ct-frame)
                (if (null? ct-frame)
                    (begin
                        (set! addr-frame (+ addr-frame 1))
                        (set! addr-offset 0)
                        (loop-env (cdr ct-env)))
                    (if (eq? var (car ct-frame))
                        (list addr-frame addr-offset)
                        (begin
                            (set! addr-offset (+ addr-offset 1))
                            (scan (cdr ct-frame))))))
            (if (null? ct-env)
                'not-found
                (scan (car ct-env))))
        (loop-env ct-env)))


(define (lexical-address-lookup env address) 
    (let* ((frame (list-ref env (addr-frame address))) 
           (value (list-ref (frame-values frame) (addr-offset address)))) 
        (if (eq? value '*unassigned*) 
                (error "the variable is unassigned -- LEXICAL-ADDRESS-LOOKUP" address) 
                value)))

(define (lexical-address-set! value env address) 
    (let ((frame (list-ref env (addr-frame address)))
          (offset (addr-offset address)))
        (define (set-value! f pos) 
            (if (= f 0) 
                    (set-car! f value) 
                    (set-value! (cdr f (- pos 1))))) 
            (set-value! frame offset value)))


;;; compile-assignment-variable
(define (compile-variable exp target linkage ct-env) 
  (let ((lexical-addr (find-variable exp ct-env))) 
    (end-with-linkage linkage 
      (make-instruction-sequence '(env) (list target) 
        (if (eq? 'not-found lexical-addr) 
            `((save env)
              (assign env (op get-global-environment))
              (assign ,target 
                      (op lookup-variable-value) 
                      (const ,exp) 
                      (reg env))
              (restore env)) 
            `((assign ,target 
                      (op lexical-address-lookup) 
                      (const ,lexical-addr) 
                      (reg env))))))))


;;; 
(define (compile-assignment exp target linkage ct-env)
  (let ((lexical-addr (find-variable exp ct-env))
        (var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next ct-env)))
    (end-with-linkage linkage
      (preserving '(env)
        get-value-code
        (make-instruction-sequence '(env val) (list target)
          (if (eq? 'not-found lexical-addr)
              `((perform (op set-variable-value!)
                         (const ,var)
                         (reg val)
                         (reg env))
                (assign ,target (const ok)))
              `((perform (op lexical-address-set!)
                         (reg val)
                         (reg env)
                         (lexical-addr))
                (assign ,target (const ok)))))))))

;;; test
(load "/home/lancecopper/code/SICP/lex-addr-compiler.scm")

(define foo
    (compile
     '((lambda (x y)
        ((lambda (a b)
          ((lambda (x w)
            (set! y x)
            (set! b w)
            (cons y b))
          a x))
        100 200))
      var1 var2)    
     'val
     'next
     '()))
(iter-display (caddr foo))

;;;
(load "/home/lancecopper/code/SICP/compiler.scm")

(define foo
    (compile
     '((lambda (x y)
        ((lambda (a b)
          ((lambda (x w)
            (set! y x)
            (set! b w)
            (cons y b))
          a x))
        100 200))
      var1 var2)    
     'val
     'next))
(iter-display (caddr foo))

;;; comparison
;;; new
(assign proc (op make-compiled-procedure) (label entry2) (reg env))
(goto (label after-lambda1))
entry2
(assign env (op compiled-procedure-env) (reg proc))
(assign env (op extend-environment) (const (x y)) (reg argl) (reg env))
(assign proc (op make-compiled-procedure) (label entry4) (reg env))
(goto (label after-lambda3))
entry4
(assign env (op compiled-procedure-env) (reg proc))
(assign env (op extend-environment) (const (a b)) (reg argl) (reg env))
(assign proc (op make-compiled-procedure) (label entry6) (reg env))
(goto (label after-lambda5))
entry6
(assign env (op compiled-procedure-env) (reg proc))
(assign env (op extend-environment) (const (x w)) (reg argl) (reg env))
(assign val (op lexical-address-lookup) (const (0 0)) (reg env))
(perform (op set-variable-value!) (const y) (reg val) (reg env))
(assign val (const ok))
(assign val (op lexical-address-lookup) (const (0 1)) (reg env))
(perform (op set-variable-value!) (const b) (reg val) (reg env))
(assign val (const ok))
(save env)
(assign env (op get-global-environment))
(assign proc (op lookup-variable-value) (const cons) (reg env))
(restore env)
(assign val (op lexical-address-lookup) (const (1 1)) (reg env))
(assign argl (op list) (reg val))
(assign val (op lexical-address-lookup) (const (2 1)) (reg env))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch9))
compiled-branch8
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch9
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(goto (reg continue))
after-call7
after-lambda5
(assign val (op lexical-address-lookup) (const (1 0)) (reg env))
(assign argl (op list) (reg val))
(assign val (op lexical-address-lookup) (const (0 0)) (reg env))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch12))
compiled-branch11
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch12
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(goto (reg continue))
after-call10
after-lambda3
(assign val (const 200))
(assign argl (op list) (reg val))
(assign val (const 100))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch15))
compiled-branch14
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch15
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(goto (reg continue))
after-call13
after-lambda1
(save env)
(assign env (op get-global-environment))
(assign val (op lookup-variable-value) (const var2) (reg env))
(restore env)
(assign argl (op list) (reg val))
(save env)
(assign env (op get-global-environment))
(assign val (op lookup-variable-value) (const var1) (reg env))
(restore env)
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch18))
compiled-branch17
(assign continue (label after-call16))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch18
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call16



;;; old one
(assign proc (op make-compiled-procedure) (label entry2) (reg env))
(goto (label after-lambda1))
entry2
(assign env (op compiled-procedure-env) (reg proc))
(assign env (op extend-environment) (const (x y)) (reg argl) (reg env))
(assign proc (op make-compiled-procedure) (label entry4) (reg env))
(goto (label after-lambda3))
entry4
(assign env (op compiled-procedure-env) (reg proc))
(assign env (op extend-environment) (const (a b)) (reg argl) (reg env))
(assign proc (op make-compiled-procedure) (label entry6) (reg env))
(goto (label after-lambda5))
entry6
(assign env (op compiled-procedure-env) (reg proc))
(assign env (op extend-environment) (const (x w)) (reg argl) (reg env))
(assign val (op lookup-variable-value) (const x) (reg env))
(perform (op set-variable-value!) (const y) (reg val) (reg env))
(assign val (const ok))
(assign val (op lookup-variable-value) (const w) (reg env))
(perform (op set-variable-value!) (const b) (reg val) (reg env))
(assign val (const ok))
(assign proc (op lookup-variable-value) (const cons) (reg env))
(assign val (op lookup-variable-value) (const b) (reg env))
(assign argl (op list) (reg val))
(assign val (op lookup-variable-value) (const y) (reg env))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch9))
compiled-branch8
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch9
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(goto (reg continue))
after-call7
after-lambda5
(assign val (op lookup-variable-value) (const x) (reg env))
(assign argl (op list) (reg val))
(assign val (op lookup-variable-value) (const a) (reg env))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch12))
compiled-branch11
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch12
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(goto (reg continue))
after-call10
after-lambda3
(assign val (const 200))
(assign argl (op list) (reg val))
(assign val (const 100))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch15))
compiled-branch14
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch15
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(goto (reg continue))
after-call13
after-lambda1
(assign val (op lookup-variable-value) (const var2) (reg env))
(assign argl (op list) (reg val))
(assign val (op lookup-variable-value) (const var1) (reg env))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch18))
compiled-branch17
(assign continue (label after-call16))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch18
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call16



