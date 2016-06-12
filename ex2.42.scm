(define nil '())

;;; The empty board 
(define empty-board nil) 

;;; Constructor and selectors for queen placements. 
(define (place-queen rank file) 
    (cons rank file))

(define (queen-rank queen) 
    (car queen))

(define (queen-file queen) 
    (cdr queen))

;;; ADJOIN-POSITION places a new queen in a board position.
(define (adjoin-position rank file board) 
   (cons (place-queen rank file) 
         board))


;;; FIND-FIRST returns the leftmost element of a list for which PRED is 
;;; true, or NIL if there are no matches. 
(define (find-first pred items)
    (cond ((null? items) nil)
          ((pred (car items))
           (car items))
          (else (find-first pred (cdr items)))))


;;; SAFE? tests if the queen in a file is safe from attack. 
(define (safe? file board)
    (define (get-queen-by-file file board)
        (find-first (lambda (queen)
                            (= (queen-file queen) file)) 
                    board))

    (let ((the-queen
            (get-queen-by-file file board)) 
          (other-queens 
            (filter (lambda (q) 
                     (not (and (= (queen-rank (get-queen-by-file file board)) 
                                  (queen-rank q)) 
                               (= (queen-file (get-queen-by-file file board)) 
                                  (queen-file q))))) 
                    board)))

         ;; To be safe, THE-QUEEN must not share with any other queen either the 
         ;; same rank… 
         (and (not (accumulate (lambda (p q) 
                                 (or q 
                                     (= (queen-rank p) 
                                        (queen-rank the-queen)))) 
                               #f 
                               other-queens)) 
  
              ;; …or the same diagonal, which is the case if 
              ;;      (= (ABS (- (QUEEN-RANK THE-QUEEN) (QUEEN-RANK ANY-QUEEN))) 
              ;;         (ABS (- (QUEEN-FILE THE-QUEEN) (QUEEN-FILE ANY-QUEEN)))) 
              (not (accumulate (lambda (p q) 
                                 (or q 
                                     (= (abs (- (queen-rank the-queen) (queen-rank p))) 
                                        (abs (- (queen-file the-queen) (queen-file p)))))) 
                               #f 
                               other-queens))))) 


;;; The main QUEENS procedure to solve the n-Queens Problem. 
(define (queens board-size) 
 (define (queen-cols k) 
   (if (= k 0) 
       (list empty-board) 
       (filter 
        (lambda (positions) (safe? k positions)) 
        (flatmap 
         (lambda (rest-of-queens) 
           (map (lambda (new-row) 
                  (adjoin-position new-row k rest-of-queens)) 
                (enumerate-interval 1 board-size))) 
         (queen-cols (- k 1)))))) 
 (queen-cols board-size)) 

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval x y)
    (define (iter x y result)
        (if (> x y)
            result
            (iter (+ x 1) y (cons x result))))
    (reverse (iter x y nil)))













