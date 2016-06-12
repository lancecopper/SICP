(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))


(define (left-branch mobile)
    (car mobile))

(define (right-branch mobile)
    (car (cdr mobile)))

(define (branch-length branch)
    (car branch))

(define (branch-structure branch)
    (car (cdr branch)))

(define (total-weight mobile)
    (if (not (pair? mobile))
        mobile
        (let ((lb-st (branch-structure (left-branch mobile))) 
              (rb-st (branch-structure (right-branch mobile))))
              (+ (total-weight lb-st) (total-weight rb-st)))))

(define x (make-mobile (make-branch 1 2) (make-branch 1 2)))
(define y (make-mobile (make-branch 2 12) (make-branch 2 12)))
(define z (make-mobile (make-branch 2 x) (make-branch 2 y)))
(define a (make-mobile (make-branch 2 z) (make-branch 2 z)))


(define (balanced? mobile)
    (if (not (pair? mobile))
        true
        (let ((lb-st (branch-structure (left-branch mobile)))
              (rb-st (branch-structure (right-branch mobile)))
              (lb-lt (branch-length (left-branch mobile)))
              (rb-lt (branch-length (right-branch mobile))))
              (and (= (* lb-lt (total-weight lb-st)) (* rb-lt (total-weight rb-st)))
                   (balanced? lb-st)
                   (balanced? rb-st)))
    ))

              
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))              



(define (left-branch mobile)
    (car mobile))

(define (right-branch mobile)
    (cdr mobile))

(define (branch-length branch)
    (car branch))

(define (branch-structure branch)
    (cdr branch))







