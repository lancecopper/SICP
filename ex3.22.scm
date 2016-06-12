

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
        (define (set-front-ptr! item) (set! front-ptr item))
        (define (set-rear-ptr! item) (set! rear-ptr item))
        (define (empty-queue?) (null? front-ptr))
        (define (front-queue)
            (if (empty-queue?)
                (error "FRONT called with an empty queue")
                (car front-ptr)))
        (define (insert-queue! item)
          (let ((new-pair (cons item '())))
            (cond ((empty-queue?)
                   (set-front-ptr! new-pair)
                   (set-rear-ptr! new-pair))
                  (else
                   (set-cdr! rear-ptr new-pair)
                   (set-rear-ptr! new-pair)))
            (print-queue)))
        (define (delete-queue!)
          (cond ((empty-queue?)
                 (error "DELETE! called with an empty queue"))
                (else
                 (set-front-ptr! (cdr front-ptr))))
          (print-queue))
        (define (print-queue)
            front-ptr)
        (define (dispatch m)
            (cond ((eq? m 'front-ptr) front-ptr)
                  ((eq? m 'rear-ptr) rear-ptr)
                  ((eq? m 'empty-queue?) empty-queue)
                  ((eq? m 'front-queue) front-queue)
                  ((eq? m 'insert-queue!) insert-queue!)
                  ((eq? m 'delete-queue!) delete-queue!)
                  ((eq? m 'print-queue) print-queue)
                  (else (error "undefined operation -- QUEUE" m))))
        dispatch))

(define (insert-queue! queue item)
    ((queue 'insert-queue!) item))
(define (delete-queue! queue)
    ((queue 'delete-queue!)))
(define (print-queue queue)
    ((queue 'print-queue)))


(define q1 (make-queue))
(insert-queue! q1 'a)
((a) a)
(insert-queue! q1 'b)
((a b) b)
(delete-queue! q1)
((b) b)
(delete-queue! q1)
(() b)
(print-queue q1)



