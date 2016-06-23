(inc (reg a))

((eq? (car inst) 'inc)
    (make-inc inst machine pc)) 

(define (inc-reg-name inst) 
 (cadr inst))
(define (make-inc inst machine pc)
    (let ((target))))

(define (make-inc inst machine pc) 
    (let ((target  
          (get-register machine (inc-reg-name inst)))) 
        (lambda () 
            (set-contents! target (+ (get-contents target) 1)) 
            (advance-pc pc)))) 
 




(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            (else (error "Unknown request -- STACK"
                         message))))
    dispatch))








