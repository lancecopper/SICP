



(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

;;;

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

;;;

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


;;; answer
(define random-init 0) 
(define (rand-update x) (+ x 1)) ; A not-very-evolved PNRG 


(define (random-numbers s-in)
  (define (action x m)
    (cond ((eq? m 'generate)
           (rand-update x))
          (else m)))
  (cons-stream 
   random-init
   (stream-map action (random-numbers s-in) s-in)))

; test
(define s-in
    (cons-stream 'generate s-in))
(define s (random-numbers s-in)) 
(test s 10)



