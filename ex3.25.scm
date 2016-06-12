
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key record)
        (cond ((not (pair? record)) false)
              ((same-key? key (caar record)) (car record))
              (else (assoc key (cdr record)))))

    (define (lookup . keys)
        (define (iter keys table)
            (let ((record (assoc (car keys) (cdr table))))
                (if record
                    (if (null? (cdr keys))
                        (cdr record)
                        (iter (cdr keys) record))
                    false)))
        (iter keys local-table))

    (define (insert! value . keys)
        (define (make-record value keys record)
            (if (null? (cdr keys))
                (cons (cons (car keys) value) record)
                (cons (cons (car keys) (make-record value (cdr keys) nil)) record)))
        (define (iter keys table)
            (let ((record (assoc (car keys) (cdr table))))
                (if record
                    (if (null? (cdr keys))
                        (set-cdr! record value)
                        (iter (cdr keys) record))
                    (set-cdr! table (make-record value keys (cdr table)))))
            'ok)
        (iter keys local-table))
  
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define nil '())
(define (same-key? x y)
    (equal? x y))

(define operation-table (make-table same-key?))

(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 10 'fruit 'melon 'watermelon)
(get 'fruit 'melon 'watermelon)
(get 'fruit 'melon)
(get 'fruit)
(put 999 'metal)
(put 999 'weapon 'gun)
(get 'metal)


(put 99 'metal 'gun)
(put 10 'fruit 'orange)






