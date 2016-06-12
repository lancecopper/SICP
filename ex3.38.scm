
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))


;;; a

35, 40, 45, 50

;;; b 

100, 90, 80, 60, 55, 50, 45, 40, 35, 30



