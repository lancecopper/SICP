;;; component problems: 
;;; cond, and, or, let, let*, letrec, unless



;;; registers: exp, env, val, continue, proc, argl, and unev

;;; cond1
eval-dispatch
... ...
  (test (op cond?) (reg exp))
    (branch (label ev-cond))
... ...

ev-cond
    (assign exp (op cond-clauses) (reg exp))
expand-clauses-loop
    (test (op null?) (reg exp))
    (branch (label null-clauses))
    (save proc)
    (assign exp (op car) (reg exp))
    (assign proc (op cdr) (reg exp))
    (test (op cond-else-clause?) (reg exp))
    (branch (label else-clause))
    (save argl)
    (assign argl (op cond-predicate) (reg exp))
    (assign exp (op cond-actions) (reg exp))
    (assign exp (op sequence->exp) (reg exp))
    (save argl)
    (save exp)
    (save continue)
    (assign continue (label make-if))
    (assign exp (op expand-clauses) (reg proc))
    (goto (label expand-clauses-loop))

make-if
    (restore argl)
    (restore exp)
    (restore continue)
    (assign exp (op make-if) (reg argl) (reg exp) (reg env))
    (restore proc)
    (restore argl)
    (goto (label ev-if))

else-clause
    (test (op null?) (reg proc))
    (branch (label null-rest))
    (restore proc)
    (perform (op error-else-clause-not-last))

null-rest
    (assign exp (op cond-actions) (reg exp))
    (assign exp (op sequence->exp) (reg exp))
    (goto (label ev-begin))

null-clauses
    (restore proc)
    (assign val (const #f))
    (goto (reg continue))

;;; cond2

eval-dispatch
... ...
  (test (op cond?) (reg exp))
    (branch (label ev-cond))
... ...

ev-cond 
  (assign exp (op cond->if) (reg exp) (reg env)) 
  (goto (label eval-dispatch)) 


;;; and
((and? exp) (eval (and->if exp) env))
;;; 

eval-dispatch
... ...
  (test (op and?) (reg exp))
    (branch (label ev-and))
... ...


;;; let
((let? exp) (eval (let->combination exp) env))
;;;
eval-dispatch
... ...
  (test (op let?) (reg exp))
    (branch (label ev-let))
... ...

ev-let 
  (assign exp (op let->combination) (reg exp) (reg env)) 
  (goto (label eval-dispatch))










