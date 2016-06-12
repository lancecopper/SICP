;;; selector, constructor
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (make-entry key value)
    (cons key value))
(define (key entry)
    (car entry))
(define (value entry)
    (cdr entry))
(define nil '())

    
   
(define (make-table)
  (let ((local-tree (list '*tree*)))
    (define (assoc given-key tree)
        (cond ((or (not (list? tree)) (null? tree)) false)
              ((= given-key (key (entry tree))) (entry tree))
              ((< given-key (key (entry tree))) (assoc given-key (left-branch tree)))
              (else (assoc given-key (right-branch tree)))))

    (define (lookup . keys)
        (define (iter keys tree)
            (let ((record (assoc (car keys) (cdr tree))))
                (if record
                    (if (null? (cdr keys))
                        (cdr record)
                        (iter (cdr keys) record))
                    false)))
        (iter keys local-tree))

    (define (insert! value . keys)
        (define (insert-node node tree)
            (if (null? tree)
                (make-tree node nil nil)
                (begin
                    (if (< (key node) (key (entry tree)))
                        (set-car! (cdr tree) (insert-node node (left-branch tree)))
                        (set-car! (cddr tree) (insert-node node (right-branch tree))))
                    tree)))
        (define (make-record value keys record)
            (if (null? (cdr keys))
                (insert-node (cons (car keys) value) record)
                (insert-node (cons (car keys) (make-record value (cdr keys) nil)) record)))
        (define (iter keys tree)
            (let ((record (assoc (car keys) (cdr tree))))
                (if record
                    (if (null? (cdr keys))
                        (set-cdr! record value)
                        (iter (cdr keys) record))
                    (set-cdr! tree (make-record value keys (cdr tree)))))
            'ok)
        (iter keys local-tree))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print-table) local-tree)
            (else (error "Unknown operation -- tree" m))))
    dispatch))


;;; test
(define operation-table (make-table))

(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define print (operation-table 'print-table))


(put 'melon 10)
(put 'fruit '10 '9)
(put 'fruit '3 '3)

(put 'metal '3 '5)

(put 'metal '3 '6)


(get 'fruit 'melon)
(get 'fruit)
(put 999 'metal)
(put 999 'weapon 'gun)
(get 'metal)


(put 99 'metal 'gun)
(put 10 'fruit 'orange)














