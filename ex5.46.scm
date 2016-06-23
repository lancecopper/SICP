(load "/home/lancecopper/code/SICP/5.57.scm")


(compile-and-go
 '(define (fib n)
    (if (< n 2)
        n
        (+ (fib (- n 1)) (fib (- n 2))))))


;;; compiled
;;; maximum-depth
3n-1
;;; ritio
3/5
;;; total-pushes
s(n)=s(n-1)+10*fib(n)
s(n)=s(n-1)+s(n-2)+3
s(n)=10fib(n+1)-3
;;; ritio
5/28


;;; interpreted
;;; maximum-depth
5n+3
;;; total-pushes
s(n)=s(n-1)+s(n-2)+40   (n>=2)
s(n)=a * fib(n+1) + b     (a=46, b=-40)
s(n)=56*fib(n+1)-40


;;; special-purpose machine
;;; maximum-depth
2n-2
;;; ritio
2/5
;;; total-pushes
s(n)=4fib(n+1)-4
;;; ritio
1/14





(define fib-machine
  (make-machine
   (list (list '= =) 
         (list '- -) 
         (list '* *) 
         (list 'read read)
         (list '< <)
         (list '+ +))
   '(controller
     loop-start
       (perform (op initialize-stack))
       (assign n (op read))
       (assign continue (label fib-done))
     fib-loop
       (test (op <) (reg n) (const 2))
       (branch (label immediate-answer))
       ;; set up to compute Fib(n - 1)
       (save continue)
       (assign continue (label afterfib-n-1))
       (save n)                           ; save old value of n
       (assign n (op -) (reg n) (const 1)); clobber n to n - 1
       (goto (label fib-loop))            ; perform recursive call
     afterfib-n-1                         ; upon return, val contains Fib(n - 1)
       (restore n)
       (restore continue)
       ;; set up to compute Fib(n - 2)
       (assign n (op -) (reg n) (const 2))
       (save continue)
       (assign continue (label afterfib-n-2))
       (save val)                         ; save Fib(n - 1)
       (goto (label fib-loop))
     afterfib-n-2                         ; upon return, val contains Fib(n - 2)
       (assign n (reg val))               ; n now contains Fib(n - 2)
       (restore val)                      ; val now contains Fib(n - 1)
       (restore continue)
       (assign val                        ;  Fib(n - 1) +  Fib(n - 2)
               (op +) (reg val) (reg n)) 
       (goto (reg continue))              ; return to caller, answer is in val
     immediate-answer
       (assign val (reg n))               ; base case:  Fib(n) = n
       (goto (reg continue))
     fib-done
       (perform (op print-stack-statistics)) 
       (goto (label loop-start)))
   ))

(start fib-machine))


;;; detail
(total-pushes = 0 maximum-depth = 0)2

(total-pushes = 4 maximum-depth = 2)3

(total-pushes = 8 maximum-depth = 4)4

(total-pushes = 16 maximum-depth = 6)5

(total-pushes = 28 maximum-depth = 8)6

(total-pushes = 48 maximum-depth = 10)7

(total-pushes = 80 maximum-depth = 12)8

(total-pushes = 132 maximum-depth = 14)9

(total-pushes = 216 maximum-depth = 16)10

(total-pushes = 352 maximum-depth = 18)
