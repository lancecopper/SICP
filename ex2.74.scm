;;; a
;;; add a tag which can indicate which division this file belong to
;;; and the funcition get-division can get the exact division from this tag

(define (get-record file-name employee_name)
    (let ((division (get-division file-name)))
        ((get 'get-record division) (get-content file-name) employee_name)))


;;; b 
;;; add a tag "division" to each record 
(define (get-salary record)
    (let ((division (get-division-from-record record)))
        ((get 'get-salary division) (get-content record))))


;;; c
(define (find-employee-record divisions employee_name)
    (if (null? divisions)
        (error "not found! employee_name----" employee_name)
        (let ((find-record (get-record (car divisions) employee_name)))
             (if find-record 
                 find-record 
                 (find-employee-record (cdr divisions) employee_name)))))


;;; d



;;; apply-generic do not suite all function

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))


(define (get-record file-name employee_name)
    (apply-generic 'get-record file-name employee_name)


(define (get-salary record)
    (apply-generic 'get-salary record))









