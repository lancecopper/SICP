;;; answer
(define (+? exp) 
    (tagged-list? exp '+)) 
;;设定只处理两个参数的情况 
(define (spread-arguments argl) 
    (let ((operand-code1 (compile (car argl) 'arg1 'next)) 
                (operand-code2 (compile (cadr argl) 'arg2 'next))) 
        (preserving '(env) 
                                operand-code1 
                                (make-instruction-sequence 
                                 (list-union '(arg1) 
                                                         (registers-needed operand-code2)) 
                                 (list-difference (registers-modified operand-code2) 
                                                                    '(arg1)) 
                                 (append '((save arg1)) 
                                                 (statements operand-code2) 
                                                 '((restore arg1)))))))
(define (compile-+ exp target linkage) 
    (let ((operand-codes (spread-arguments (operands exp)))) 
        (end-with-linkage 
         linkage 
         (preserving '(continue) 
                                 operand-codes 
                                 (make-instruction-sequence 
                                    '() 
                                    `(target) 
                                    `((assign ,target (op +) (reg arg1) (reg arg2))))))))

;;; test
(load "/home/lancecopper/code/SICP/ex5.38-compiler.scm")
(define foo 
        (compile 
        '(+ (+ a 1) (+ 3 2))
        'val
        'next))

(iter-display (caddr foo))

;;; assembly code
    (assign arg1 (op lookup-variable-value) (const a) (reg env))
    (save arg1)
    (assign arg2 (const 1))
    (restore arg1)
    (assign arg1 (op +) (reg arg1) (reg arg2))
    (save arg1)
    (assign arg1 (const 3))
    (save arg1)
    (assign arg2 (const 2))
    (restore arg1)
    (assign arg2 (op +) (reg arg1) (reg arg2))
    (restore arg1)
    (assign val (op +) (reg arg1) (reg arg2))

;;; comparison
(load "/home/lancecopper/code/SICP/compiler.scm")
(define foo 
        (compile 
        '(+ (+ a 1) (+ 3 2))
        'val
        'next))

(iter-display (caddr foo))
;;; assembly instruction
    (assign proc (op lookup-variable-value) (const +) (reg env))
    (save proc)
    (save env)
    (assign proc (op lookup-variable-value) (const +) (reg env))
    (assign val (const 2))
    (assign argl (op list) (reg val))
    (assign val (const 3))
    (assign argl (op cons) (reg val) (reg argl))
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch6))
compiled-branch5
    (assign continue (label after-call4))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
primitive-branch6
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call4
    (assign argl (op list) (reg val))
    (restore env)
    (save argl)
    (assign proc (op lookup-variable-value) (const +) (reg env))
    (assign val (const 1))
    (assign argl (op list) (reg val))
    (assign val (op lookup-variable-value) (const a) (reg env))
    (assign argl (op cons) (reg val) (reg argl))
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch3))
compiled-branch2
    (assign continue (label after-call1))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
primitive-branch3
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call1
    (restore argl)
    (assign argl (op cons) (reg val) (reg argl))
    (restore proc)
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch9))
compiled-branch8
    (assign continue (label after-call7))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
primitive-branch9
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call7



;;d 
(define (compile-++ exp target linkage) 
    (compile-+ (construct exp) target linkage)) 
(define (construct exp) 
    (if (> (length (operands exp)) 
                 2) 
            (append (list (car exp) 
                                        (cadr exp)) 
                            (list (append (list (car exp)) 
                                                        (cddr exp)))) 
            exp)) 
                

