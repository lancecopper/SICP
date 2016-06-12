
;;; make-account
(define (make-account balance secret-password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (incorrect-password amount)
    "Incorrect password")
  (define (dispatch input-password m)
    (if (eq? input-password secret-password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT")))
        incorrect-password))
  dispatch)

;;; Make-joint

(define (Make-joint account password1 password2)
    (define (incorrect-password amount)
        "Incorrect password")
    (define (dispatch input-password m)
        (if (eq? input-password password2)
            (account password1 m)
            incorrect-password))
    dispatch)


;;; test
(define peter-acc (make-account 100 'open-sesame))


(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))


((paul-acc 'rosebud 'withdraw) 40)
60

((paul-acc 'rosebud 'withdraw) 40)

((paul-acc 'some-other-password 'deposit) 50)
"Incorrect password"


