;;; selector, constructor
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))


;;;
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

(define (lookup given-key tree)
    (cond ((null? tree) false)
          ((= given-key (key (entry tree))) (entry tree))
          ((< given-key (key (entry tree))) (lookup given-key (left-branch tree)))
          (else (lookup given-key (right-branch tree)))))












