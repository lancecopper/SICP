
;;; a 




(define (eval exp env)
; ......
        ((letrec? exp) (eval (letrec->let exp) env))
; ......
)

(define (letrec? exp) (tagged-list? exp 'letrec))
(define (make-unassigned-letrec vars)
  (if (null? vars)
      '()
      (cons (list (car vars) '*unassigned*) 
            (make-unassigned-letrec (cdr vars)))))
(define (make-set-letrec vars exps)
  (if (null? vars)
      '()
      (cons (list 'set! (car vars) (car exps))
            (make-set-letrec (cdr vars) (cdr exps)))))
(define (letrec->let exp)
  (let* ((assi (let-assignment exp))
         (lvars (let-var assi))
         (lexps (let-exp assi)))
    (cons 'let (cons (make-unassigned-letrec lvars)
                     (append (make-set-letrec lvars lexps)
                             (let-body exp))))))


(letrec ((x 1)
         (y (+ x 1)))
        (+ x y))


(define (test x y)
    (define (foo a b)
        (set! x (+ 10 y))
        (set! y (+ 10 x))
        (list a b x y))
    (foo 0 0))

(test 1 2)

(lambda (n)
    (if (= n 0)
        true
            (odd? (- n 1))))


