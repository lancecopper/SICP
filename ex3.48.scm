;;; serializer
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define nil '())

(define (make-mutex)
  (let ((cell (list false)))            
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))
;;;
(define uniq-account-num
    (let ((uniq-num 0))
         (define (new-uniq-num)
             (set! uniq-num (+ 1 uniq-num))
             uniq-num)
         new-uniq-num))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer))
        (uniq-num (uniq-account-num)))
       (define (dispatch m)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                ((eq? m 'balance) balance)
                ((eq? m 'uniq-num) uniq-num)
                ((eq? m 'serializer) balance-serializer)
                (else (error "Unknown request -- MAKE-ACCOUNT"
                             m))))
    dispatch))


;;; test
(define a1 (make-account 100))
(define a2 (make-account 200))

(a1 'uniq-num)
(a2 'uniq-num)


(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer))
        (uniq-num1 (account1 'uniq-num))
        (uniq-num2 (account2 'uniq-num)))
    (if (> uniq-num1 uniq-num2)
        ((serializer1 (serializer2 exchange))
         account1
         account2)
        ((serializer2 (serializer1 exchange))
         account1
         account2))))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(a1 'balance)
(a2 'balance)
(serialized-exchange a1 a2)
(a1 'balance)
(a2 'balance)
