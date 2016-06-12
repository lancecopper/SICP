;;; old eval
(load "/home/lancecopper/code/SICP/old-eval-apply.scm")
(load "/home/lancecopper/code/SICP/old-env-primitive.scm")

;;; new eval
(load "/home/lancecopper/code/SICP/amb-analyze.scm")
(driver-loop)


;;; test
(driver-loop)
(amb 1 2 3 4)
try-again

(driver-loop)
(ramb 1 2 3 4)
try-again



;;; answer, has been integrated into "amb-analyze.scm"

;;; ramb
(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (cdr exp))

(define (rambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze-ramb exp)
  (let ((cprocs (map analyze (ramb-choices exp))))
    (lambda (env succeed fail)
      (define (remove-choice choices choice)
          (if (eq? choice (car choices))
              (cdr choices)
              (cons (car choices) (remove-choice (cdr choices) choice))))
      (define (try-next choices)
        (if (null? choices)
            (fail)
            (let* ((rand-choice (list-ref choices (random (length choices))))
                   (rest-choices (remove-choice choices rand-choice)))
                  (rand-choice env
                               succeed
                               (lambda ()
                                 (try-next rest-choices))))))
      (try-next cprocs))))












