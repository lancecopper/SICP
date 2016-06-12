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
        '"Incorrect password")
    (define (call-the-cops amount)
        '"call the cops!")
    (define times 0)
    (define (how-many-calls?)
        times)
    (define (reset-count)
        (set! times 0)
        times)
    (define (dispatch input-password m)
        (if (eq? input-password secret-password)
            (begin
                (reset-count)
                (cond ((eq? m 'withdraw) withdraw)
                    ((eq? m 'deposit) deposit)
                    (else (error "Unknown request -- MAKE-ACCOUNT"))))
            (begin
                (set! times (+ times 1))
                (if (> times 7)
                    call-the-cops
                    incorrect-password))))
    dispatch)


;;; test
(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
60

((acc 'some-other-password 'deposit) 50)
"Incorrect password"




