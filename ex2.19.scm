(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (first-denomination items)
    (car items))

(define (except-first-denomination items)
    (cdr items))

(define (no-more? items)
    (null? items))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))


(define us-coins1 (reverse (list 50 25 10 5 1)))
(cc 100 us-coins)
(cc 100.0 uk-coins)

(define (timed-cc amount coin-values start-time) 
   (cc amount coin-values) 
   (- (runtime) start-time))

(timed-cc 200 us-coins (runtime))
(timed-cc 200 us-coins1 (runtime))


; For the last part of the exercise. 
; The order of the coins does not affect the result.
; Becuase the procedure computes all possible combinations. 
; But it does affect the speed of the computation. 
; If you start with the lower valued coins, it'll take much longer.