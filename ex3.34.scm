(define (squarer a b)
  (multiplier a a b))


;;; test
(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(multiplier a b c)


(squarer a b)


(set-value! a 3 'lance)
(set-value! b 9 'lance)



(get-value a)
(get-value b)


(get-value c)

(forget-value! b 'lance)
(forget-value! a 'lance)







