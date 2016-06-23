
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
;;; cond1
eval-dispatch
... ...
  (test (op cond?) (reg exp))
    (branch (label ev-cond))
... ...


ev-cond
    (assign exp (op cond-clauses) (reg exp))
ev-clauses-loop
    (test (op null?) (reg exp))
    (branch (label null-clauses))
    (save exp)
ev-first-clause
    (assign exp (op car) (reg exp))
    (test (op cond-else-clause?) (reg exp))
    (branch (label else-clause))
    (save env)
    (save continue)
    (assign continue (label ev-cond-decide))
    (assign exp (op cond-predicate) (reg exp))
    (goto (label eval-dispatch))
ev-cond-decide
    (restore continue)
    (restore env)
    (test (op true?) (reg val))
    (branch (label ev-cond-actions))
    (restore exp)
    (assign exp (op cdr) (reg exp))
    (goto ev-clauses-loop)
else-clause
    (restore exp)
    (save exp)
    (assign exp (op cdr) exp)
    (test (op null?) (reg exp))
    (branch (label ev-cond-actions))
    (perform (op error-else-clause-not-last))
ev-cond-actions
    (restore exp)
    (assign exp (op car) exp)
    (assign exp (op cond-actions) (reg exp))
    (goto (label sequence->exp))
null-clauses
    (assign val (const #f))
    (goto (reg continue))
sequence->exp
    (test (op null?) (reg exp))
    (branch (label null-seq))
    (test (op last-exp?) (reg exp))
    (branch (label last-exp-seq))
    (save continue)
    (goto (label ev-sequence))
null-seq
    (assign val (reg exp))
    (goto (reg continue))
last-exp-seq
    (assign exp (op first-exp) (reg exp))
    (goto (reg eval-dispatch))

