(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define rand
    (let ((key-var random-init))
        (define (generate)
            (set! key-var (rand-update key-var))
            key-var)
        (define (reset x)
            (set! key-var x)
            key-var)
        (define (dispatch m)
            (cond ((eq? m 'generate) (generate))
                  ((eq? m 'reset) reset)
                  (else (error "Unknown request -- MAKE-ACCOUNT"))))
    dispatch))

(define random-init 0) 
(define (rand-update x) (+ x 1)) ; A not-very-evolved PNRG 

(rand 'generate)
((rand 'reset) 10)


