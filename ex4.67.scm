I’ll come back to this after section 4.4.4


;;; answer1
;;; Loop detection history
(define history '())

(define (reset-history! history)
    (set! history '()))

(define (history-ref value history)
    (if (null? history)
        #f
        (if (equal? (car history) value)
            #t
            (history-ref vlaue (cdr history)))))

(define (history-add! value history)
    (cons value history))

;; Get the canonical name for a variable
;; when rules are applied new variables are generated - see:
;;    apply-a-rule -&gt; rename-variable-in -&gt; make-new-variable
;; so ?who becomes the variable (? who) which becomes (? 1 who) then (? 2 who) ...
;; the canonical name is (? who)

(define (canonical-name var)
  (if (number? (cadr var))
      (list (car var) (caddr var))
      var))

;;; change apply-a-rule

(define (apply-a-rule rule query-pattern query-frame)
  (let* ((clean-rule (rename-variables-in rule))
         (unify-result (unify-match query-pattern
                                    (conclusion clean-rule)
                                    query-frame)))
      (if (eq? unify-result 'failed)
          the-empty-stream
          (let ((instance (instantiate query-pattern
                            query-frame
                            (lambda (var frame)
                              (canonical-name var)))))
            (if (history-ref instance)
                the-empty-stream
                (begin
                  (history-add! instance)
                  (qeval (rule-body clean-rule)
                         (singleton-stream unify-result))))))))

;;; Also add a call to reset the query pattern 
;;; history to the driver loop.
;;; I added a convenient interpret-query 
;;; method to save typing into the driver loop 
;;; which needs the call too.

(define (query-driver-loop)
  (reset-history!)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate q
                            frame
                            (lambda (v f)
                              (contract-question-mark v))))
             (qeval q (singleton-stream '()))))
           (query-driver-loop)))))

;;; test
(load "/home/lancecopper/code/SICP/loop-detec-interpreter.scm")
(query-driver-loop)



(load "/home/lancecopper/code/SICP/logic-interpreter.scm")
(query-driver-loop)


;;; load job-assert!.scm 


;;; test rule
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by ?middle-manager ?boss)
               (supervisor ?staff-person ?middle-manager))))


(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by ?middle-manager ?boss)
               (supervisor ?staff-person ?middle-manager))))


;;;
outranked-by ?staff-person ?boss)


