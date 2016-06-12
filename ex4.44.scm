(define (enumerate-interval low high) 
   (if (> low high) 
       '() 
       (cons low (enumerate-interval (+ low 1) high)))) 
  
(define (attack? row1 col1 row2 col2) 
  (or (= row1 row2) 
      (= col1 col2) 
      (= (abs (- row1 row2)) (abs (- col1 col2))))) 
 
;; positions is the list of row of former k-1 queens 
(define (safe? k positions) 
  (let ((kth-row (list-ref positions (- k 1)))) 
    (define (safe-iter p col) 
      (if (>= col k) 
          true 
          (if (attack? kth-row k (car p) col) 
              false 
              (safe-iter (cdr p) (+ col 1))))) 
    (safe-iter positions 1))) 
 

(define (list-amb li) 
  (if (null? li) 
      (amb) 
      (amb (car li) (list-amb (cdr li))))) 
 
(define (queens board-size) 
  (define (queen-iter k positions) 
    (if (= k board-size) 
        positions 
        (let ((row (list-amb (enumerate-interval 1 board-size)))) 
          (let((new-pos (append positions (list row)))) 
            (require (safe? k new-pos)) 
            (queen-iter (+ k 1) new-pos))))) 
  (queen-iter 1 '())) 



;;; another solution
(define (an-integer-between a b) 
     (require (<= a b)) 
     (amb a (an-integer-between (+ a 1) b))) 
  
 ;;check if (car solution) is compatible with any of (cdr solution) 
 (define (safe? solution)  
     (let ((p (car solution))) 
         (define (conflict? q i) 
             (or 
                 (= p q) 
                 (= p (+ q i)) 
                 (= p (- q i)))) 
         (define (check rest i) 
             (cond  
                 ((null? rest) #t) 
                 ((conflict? (car rest) i) #f) 
                 (else (check (cdr rest) (inc i))))) 
         (check (cdr solution) 1))) 
  
 (define (queens n) 
     (define (iter solution n-left) 
         (if (= n-left 0) 
             (begin 
                 (display solution) 
                 (newline)) 
             (begin 
                 (let ((x-solution (cons (an-integer-between 1 n) solution))) 
                     (require (safe? x-solution)) 
                     (iter x-solution (- n-left 1)))))) 
     (iter '() n)) 
  
 (queens 8) 




