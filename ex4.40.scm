

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))


;;;

(define (multiple-dwelling)
  (let ((cooper (amb 2 3 4 5))
        (miller (amb 1 2 3 4 5)))
    (require (> miller cooper))
    (let ((fletcher (amb 2 3 4)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (let ((smith (amb 1 2 3 4 5)))
        (require (not (= (abs (- smith fletcher)) 1)))
        (let ((baker (amb 1 2 3 4)))
          (require (distinct? (list baker cooper fletcher miller smith)))
          (list (list 'baker baker)
                (list 'cooper cooper)
                (list 'fletcher fletcher)
                (list 'miller miller)
                (list 'smith smith)))))))
 


(define tests 100)
(collect-garbage)
(time 
 (let next ((x tests))
   (cond ((zero? x) 'done)
         (else (multiple-dwelling)
               (next (- x 1))))))
; cpu time: 500 real time: 500 gc time: 437
 
(collect-garbage)
(time 
 (let next ((x tests))
   (cond ((zero? x) 'done)
         (else (multiple-dwelling-original)
               (next (- x 1))))))
 
; cpu time: 1830 real time: 1830 gc time: 704


(define (multiple-dwelling)
  (let* ((fletcher (amb 2 3 4))  
         (cooper (an-element-of (filter (lambda (x)
                                          (not (= (abs (- fletcher x)) 1)))
                                        (list 2 3 4 5))))
         (miller (an-element-of (filter (lambda (x)
                                          (> x cooper))
                                        (list 1 2 3 4 5))))
         (smith (an-element-of (filter (lambda (x)
                                         (not (= (abs (- fletcher x)) 1)))
                                       (list 1 2 3 4 5))))
         (baker (amb 1 2 3 4)))
    (require (distinct? (list baker cooper fletcher miller smith)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))



