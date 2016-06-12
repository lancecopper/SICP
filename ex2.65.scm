;;; selector, constructor
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))


;;; convert a binary tree to a list
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))



;;; coverts an ordered list to a balanced binary tree.
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))



;;; auxiliary function

(define (union-set set1 set2)
    (define (iter s1 s2 result)
        (cond ((null? s1) (append result s2))
              ((null? s2) (append result s1))
              ((= (car s1) (car s2))
                (iter (cdr s1) (cdr s2) (append result (list (car s1)))))
              ((< (car s1) (car s2))
                (iter (cdr s1) s2 (append result (list (car s1)))))
              (else
                (iter s1 (cdr s2) (append result (list (car s2)))))))
    (iter set1 set2 '()))


(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

;;; answer

(define (union-set tree1 tree2) 
   (list->tree (union-set (tree->list tree1) 
                          (tree->list tree2)))) 


(define (intersection-set tree1 tree2) 
   (list->tree (intersection-set (tree->list tree1) 
                                 (tree->list tree2)))) 


































