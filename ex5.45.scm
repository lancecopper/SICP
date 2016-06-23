(load "/home/lancecopper/code/SICP/5.57.scm")

;;; 
(compile-and-go
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n))))

(factorial 10)


;;; ex5.14 with machine 
;;; ritio to ori
2/5
;;; total-pushes
2-2
;;; ritio to ori
1/16


;;; ori
;;; maximum-depth
5n+3
;;; total-pushes
32n-16



;;; optimized
;;; maximum-depth
3n-1
;;; ritio to ori
3/5
;;; total-pushes
6n+1
;;; ritio to ori
3/16


;;; code analysis
(define foo
  (compile 
    '(define (factorial n)
      (if (= n 1)
          1
          (* (factorial (- n 1)) n)))
    'val
    'next))

(iter-display (caddr foo))



;;; compile
    (assign val (op make-compiled-procedure) (label entry36) (reg env))
    (goto (label after-lambda35))
entry36
    (assign env (op compiled-procedure-env) (reg proc))
    (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
    (save continue)
    (save env)
    (assign proc (op lookup-variable-value) (const =) (reg env))
    (assign val (const 1))
    (assign argl (op list) (reg val))
    (assign val (op lookup-variable-value) (const n) (reg env))
    (assign argl (op cons) (reg val) (reg argl))
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch51))
compiled-branch50
    (assign continue (label after-call49))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
primitive-branch51
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call49
    (restore env)
    (restore continue)
    (test (op false?) (reg val))
    (branch (label false-branch38))
true-branch39
    (assign val (const 1))
    (goto (reg continue))
false-branch38
    (assign proc (op lookup-variable-value) (const *) (reg env))
    (save continue)
    (save proc)
    (assign val (op lookup-variable-value) (const n) (reg env))
    (assign argl (op list) (reg val))
    (save argl)
    (assign proc (op lookup-variable-value) (const factorial) (reg env))
    (save proc)
    (assign proc (op lookup-variable-value) (const -) (reg env))
    (assign val (const 1))
    (assign argl (op list) (reg val))
    (assign val (op lookup-variable-value) (const n) (reg env))
    (assign argl (op cons) (reg val) (reg argl))
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch42))
compiled-branch41
    (assign continue (label after-call40))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
primitive-branch42
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call40
    (assign argl (op list) (reg val))
    (restore proc)
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch45))
compiled-branch44
    (assign continue (label after-call43))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
primitive-branch45
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call43
    (restore argl)
    (assign argl (op cons) (reg val) (reg argl))
    (restore proc)
    (restore continue)
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch48))
compiled-branch47
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
primitive-branch48
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    (goto (reg continue))
after-call46
after-if37
after-lambda35
    (perform (op define-variable!) (const factorial) (reg val) (reg env))
    (assign val (const ok))
;;; special-purpose machine
loop-start
       (perform (op initialize-stack))
       (assign n (op read))
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





