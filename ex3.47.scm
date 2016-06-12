

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))


(define nil '())



(define (make-mutex)
  (let ((cell (list false)))            
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))


(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))


; a: based on mutexes
(define (make-semaphore n)
  (let ((the-mutex (make-mutex))
        (count 0))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (the-mutex 'acquire)
             (if (= count n)
                 (begin
                   (the-mutex 'release)
                   (the-semaphore 'acquire))
                 (begin
                   (set! count (+ count 1))
                   (the-mutex 'release))))
            ((eq? m 'release)
             (the-mutex 'acquire)
             (if (> count 0)
                 (set! count (- count 1)))
             (the-mutex 'release))))
    the-semaphore))





; b: based on atomic test-and-set! operations
(define (make-semaphore n)
  (let ((count-lock (list false))
        (count 0))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (acquire-count-lock)
             (if (= count n)
                 (begin
                   (release-count-lock)
                   (the-semaphore 'acquire))
                 (begin
                   (set! count (+ count 1))
                   (release-count-lock))))
            ((eq? m 'release)
             (acquire-count-lock)
             (if (> count 0)
                 (set! count (- count 1)))
             (release-count-lock))))
    (define (acquire-count-lock)
      (if (test-and-set! count-lock)
          (acquire-count-lock)))
    (define (clear! count-lock)
      (set-car! count-lock false))
    (define (test-and-set! count-lock)
      ; atmoic opeartions behaving as the following
      (if (car count-lock)
          true
          (begin (set-car! count-lock true)
                 false)))
    the-semaphore))