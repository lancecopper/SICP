(define nil '())
(define (make-deque-node item)
    (cons item (cons nil nil)))


(define (make-deque)
    (cons nil nil))
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (empty-deque? deque) (null? (front-ptr deque)))

;;;
(define (front-deque deque)
    (if (empty-deque deque)
        (error "FRONT called with an empty deque" deque)
        (car (front-ptr deque))))

(define (rear-deque deque)
    (if (empty-deque deque)
        (error "REAR called with an empty deque")
        (car (rear-ptr deque))))

(define (front-insert-deque! deque item)
    (let ((new-pair (make-deque-node item)))
        (cond ((empty-deque? deque)
                (set-front-ptr! deque new-pair)
                (set-rear-ptr! deque new-pair)
                (print-deque deque))
              (else
                (set-cdr! (cdr new-pair) (front-ptr deque))
                (set-car! (cdr (front-ptr deque)) new-pair)
               (set-front-ptr! deque new-pair)
               (print-deque deque)))))


(define (rear-insert-deque! deque item)
    (let ((new-pair (make-deque-node item)))
        (cond ((empty-deque? deque)
                (set-front-ptr! deque new-pair)
                (set-rear-ptr! deque new-pair)
                (print-deque deque))
              (else
                (set-car! (cdr new-pair) (rear-ptr deque))
                (set-cdr! (cdr (rear-ptr deque)) new-pair)
               (set-rear-ptr! deque new-pair)
               (print-deque deque)))))



(define (front-delete-deque! deque)
    (cond ((empty-deque? deque)
            (error "DELETE! called with an empty deque"))
          (else
            (set-front-ptr! deque (cddr (front-ptr deque)))
            (set-car! (cdr (front-ptr deque)) nil)
            (print-deque deque))))


(define (rear-delete-deque! deque)
    (cond ((empty-deque? deque)
            (error "DELETE! called with an empty deque"))
          (else
            (set-rear-ptr! deque (cadr (rear-ptr deque)))
            (set-cdr! (cdr (rear-ptr deque)) nil)
            (print-deque deque))))


(define (print-deque deque)
    (define (iter node)
        (if (not (null? node))
            (begin 
                (display (car node))
                (display " ")
                (iter (cddr node)))
            'okay))
    (iter (car deque)))


;;; test

(define q1 (make-deque))

(front-insert-deque! q1 'a)

(rear-insert-deque! q1 'b)

(front-insert-deque! q1 'b)

(rear-insert-deque! q1 'a)

(rear-delete-deque! q1)

(front-delete-deque! q1)

(print-deque q1)



