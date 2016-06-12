;;; a

; This code would never end because the initial 
; values i = j = k = low. The solution is k = √2 
; low which is not an integer. This will cause 
; the require to fail, an-integer-starting-from 
; will then backtrack to the next possible value of k, 
; which of course will also fail and since there is 
; no limit on k the procedure will never complete. 
; Even if we set low to 3 and start j from i+1 the first 
; solution of (3,4,5) will be found, but on calling 
; try-again for another solution, the same situation occurs.


(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (a-pythagorean-triple-from low)
  (let ((i (an-integer-starting-from low)))
    (let ((j (an-integer-starting-from i)))
      (let ((k (an-integer-starting-from j)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))


;;; b

(define (a-pythagorean-triple-from low)
    (let ((i (an-integer-starting-from low)))
        (let ((j (an-integer-starting-from i)))
            (let ((k (+ (* i i) (* j j))))
                (require (integer? k))
                (list i j k)))))


;;; this method do not produce all pythagorean triple for the
;;; restriction of arithmetic itself
(define (a-pythagorean-triple-from low)
  (define (limit i) (if (odd? i)
                        (/ (- (square i) 1) 2)
                        (- (/ (square i) 4) 1)))
  (let* ((i (an-integer-starting-from low))
         (j (an-integer-between i (limit i)))
         (k (sqrt (+ (square i) (square j)))))
    (require (integer? k))
    (list i j k)))


;;; 
(define (a-pythagorean-triple-greater-than low)
    (let ((k (an-integer-starting-from low))) 
        (let ((i (an-integer-between low k))) 
            (let ((j (an-integer-between i k))) 
                 (require (= (+ (* i i) (* j j)) (* k k))) 
                 (list i j k)))))
