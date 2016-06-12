
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;;; answer


(define operation-table make-table) 
(define get (operation-table 'lookup-proc)) 
(define put (operation-table 'insert-proc)) 
 
(put 'eval 'quote (lambda (x y) (text-of-quotation x))) 
(put 'eval 'set! eval-assignment) 
(put 'eval 'define eval-definition) 
(put 'eval 'if eval-if) 
(put 'eval 'lambda (lambda (x y) (make-procedure (lambda-parameters x) (lambda-body x) y))) 
(put 'eval 'begin (lambda (x y) (eval-sequence (begin-sequence x) y))) 
(put 'eval 'cond (lambda (x y) (eval (cond->if x) y))) 
 
(define (eval expr env) 
        (cond ((self-evaluating? expr) expr) 
              ((variable? expr) (lookup-variable-value expr env))
              ((get 'eval (car expr)) (apply (get 'eval (car expr)) expr env))) 
              ((application? expr) 
                (apply 
                    (eval (operator expr) env) 
                    (list-of-values (operands expr) env))) 
              (else  
               (error "Unkown expression type -- EVAL" expr)))) 




