;;;
(fab 3)

; n=3
...
(save continue)     ;;; (continue:((label fact-done)))
(save n)            ;;; (continue:((label fact-done)),
                    ;;;  n:(3))
...
; n=2
...
(save continue)     ;;; (continue:((label fact-done),
                    ;;;            (label after-fact)),
                    ;;; n:(3))
(save n)            ;;; (continue:((label fact-done),
                    ;;;            (label after-fact)),
                    ;;; n:(3,3))
...
; n=1
...
(restore n)         ;;; (continue:((label fact-done),
                    ;;;            (label after-fact)),
                    ;;; n:(3))
(restore continue)  ;;; (continue:((label fact-done)),
                    ;;;  n:(3))

...
; n=2
...
(restore n)         ;;; (continue:((label fact-done)))
(restore continue)  ;;;
...
fact-done




;;;
(fib 3)

...
;;;fib-loop,n=3, continue=(label fib-done)
(save continue)     ;;; (label fib-done)
continue=(label afterfib-n-1)
(save n)            ;;; 3
n=2

;;;fib-loop,n=2,continue=(label afterfib-n-1)
(save continue)     ;;; (label fib-done),(label afterfib-n-1)
continue=(label afterfib-n-1)
(save n)            ;;; 3,2
n=1
;;;fib-loop,n=1,continue=(label afterfib-n-1)
;;;immediate-answer
val=1
;;;afterfib-n-1
(restore n)         ;;; 3
(restore continue)  ;;; (label fib-done)
n=0
(save continue)     ;;; (label fib-done),(label afterfib-n-1)
continue=(label afterfib-n-2)
(save val)          ;;; 1

;;;fib-loop
;;;immediate-answer
val=0
;;;afterfib-n-2
n=0
(restore val)       ;;;
(restore continue)  ;;; (label fib-done)
continue=(label afterfib-n-1)
val=1+0=1

;;;afterfib-n-1
(restore n)         ;;;
n=3
(restore continue)  ;;;
continue=(label fib-done)
n=1
(save continue)     ;;; (label fib-done)
continue = (label afterfib-n-2)
(save val)          ;;; 1

;;;fib-loop
;;;immediate-answer
val=1
;;;afterfib-n-2
n=1
(restore val)       ;;;
val=1
(restore continue)  ;;;
continue= (label fib-done)
val=1+1=2

;;; fib-done
val=2




done



