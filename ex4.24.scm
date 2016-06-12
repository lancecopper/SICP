;;; old eval
(load "/home/lancecopper/code/SICP/old-eval-apply.scm")
(load "/home/lancecopper/code/SICP/old-env-primitive.scm")

(driver-loop)



;;; new eval
#(load "/home/lancecopper/code/SICP/new-analyze.scm")

(driver-loop)






;;; test
(driver-loop)

(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

(let ((t0 0) (t1 0))
  (define (loop i n)
    (factorial 800)
    (if (< i n)
        (loop (+ i 1) n)))
  (set! t0 (get-universal-time))
  (loop 0 100)
  (set! t1 (get-universal-time))
  (- t1 t0))


;;; result
9 : 16





;;; rubbish



(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))


(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)


(define expr '(define (foo x) (+ x 1)))
(define var (definition-variable expr))
(define pro-vproc (definition-value expr))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(define lambda-vars (lambda-parameters pro-vproc))
(define pro-bproc (lambda-body pro-vproc))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))


(define lambda-body1 (lambda-body pro-vproc))
(define bproc (analyze-sequence lambda-body1))

(define bproc (analyze-sequence (lambda-body pro-vproc)))

(define env the-global-environment)

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(make-procedure lambda-vars bproc env)

(lambda (env) (make-procedure vars bproc env))


(define lambda-pro (analyze-lambda pro-vproc))
(define vproc (analyze (definition-value expr)))



