

;;; a
(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

(load "/home/lancecopper/code/SICP/machine-model5.19.scm")

(define count-leaves-machine
  (make-machine
   (list (list 'null? null?) 
         (list 'pair? pair?) 
         (list 'car car) 
         (list 'cdr cdr)
         (list '+ + )
         (list 'done (lambda () (newline) (display "done"))))
   '(controller
       (assign continue (label count-leaves-done))
    count-leaves-loop
       (test (op null?) (reg tree))
       (branch (label null-value))
       (test (op pair?) (reg tree))
       (branch (label pair-value))
       (assign val (const 1))
       (goto (reg continue))
    pair-value
       (save continue)
       (assign continue (label after-leave-car))
       (save tree)
       (assign tree (op car) (reg tree))
       (goto (label count-leaves-loop))
    after-leave-car
       (restore tree)
       (assign tree (op cdr) (reg tree))
       (assign continue (label after-leave-cdr))
       (save val)
       (goto (label count-leaves-loop))
    after-leave-cdr
       (assign temp (reg val))
       (restore val)
       (restore continue)
       (assign val (op +) (reg val) (reg temp))
       (goto (reg continue))
    null-value
       (assign val (const 0))
       (goto (reg continue))
    count-leaves-done
       (perform (op done)))))

(define test-tree (list (list 1 2 3 4) (cons 5 6) (list 7) (cons 8 (cons 9 10))))

(set-register-contents! count-leaves-machine 'tree  test-tree)

(start count-leaves-machine)

(get-register-contents count-leaves-machine 'val)



;;; b
(define (count-leaves tree)
  (define (count-iter tree n)
    (cond ((null? tree) n)
          ((not (pair? tree)) (+ n 1))
          (else (count-iter (cdr tree)
                            (count-iter (car tree) n)))))
  (count-iter tree 0))

(define count-leaves-machine1
  (make-machine
   (list (list 'null? null?) 
         (list 'pair? pair?) 
         (list 'car car) 
         (list 'cdr cdr)
         (list '+ +)
         (list 'done (lambda () (newline) (display "done"))))
   '(controller
       (assign continue (label count-leaves-done))
       (assign n (const 0))
    count-leaves-loop
       (test (op null?) (reg tree))
       (branch (label null-value))
       (test (op pair?) (reg tree))
       (branch (label pair-value))
       (assign n (op +) (reg n) (const 1))
       (goto (reg continue))
    pair-value
       (save continue)
       (assign continue (label after-leave-car))
       (save tree)
       (assign tree (op car) (reg tree))
       (goto (label count-leaves-loop))
    after-leave-car
       (restore tree)
       (assign tree (op cdr) (reg tree))
       (assign continue (label after-leave-cdr))
       (goto (label count-leaves-loop))
    after-leave-cdr
       (restore continue)
       (goto (reg continue))
    null-value
       (goto (reg continue))
    count-leaves-done
       (perform (op done)))))

(define test-tree (list (list 1 2 3 4) (cons 5 6) (list 7) (cons 8 (cons 9 10))))

(set-register-contents! count-leaves-machine1 'tree test-tree)

(start count-leaves-machine1)

(get-register-contents count-leaves-machine1 'n)

(get-register-contents count-leaves-machine1 'tree)
