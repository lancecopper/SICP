

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
(define (remove x ls)
    (if (= x (car ls))
        (cdr ls)
        (cons (car ls) (remove x (cdr ls)))))

(define (flatmap proc li) 
   (if (null? li) 
       '() 
       (let ((result (proc (car li))) 
             (rest (flatmap proc (cdr li)))) 
         (if (pair? result) 
             (append result rest) 
             (cons result rest))))) 

(define (multiple-dwelling)
    (define (iter-arrange list-n)
        (flatmap 
            (lambda 
                (x) 
                (let ((next (remove x list-n)))
                     (if (null? next)
                         (list (list x))
                         (map 
                            (lambda (p) (cons x p)) 
                            (iter-arrange next)))))
            list-n))
    (define ori-multiple-dwelling (iter-arrange (list 1 2 3 4 5)))
    (define (invalid-solution permutation)
        (let ((baker (first permutation))
              (cooper (second permutation))
              (fletcher (third permutation))
              (miller (fourth permutation))
              (smith (fifth permutation)))
          (and (not (= baker 5))
               (not (= cooper 1))
               (not (= fletcher 5))
               (not (= fletcher 1))
               (> miller cooper)
               (not (= (abs (- smith fletcher)) 1))
               (not (= (abs (- fletcher cooper)) 1)))))
    (define (present-solution solution)
        (map
            (lambda (x)
                (map list 
                     '(baker cooper fletcher miller smith)
                     x))
            solution))
    (present-solution
        (filter invalid-solution ori-multiple-dwelling)))



