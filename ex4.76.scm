(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))

(put 'and 'qeval conjoin)




(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame))  ; ***
        ((and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
           (unify-match
            (binding-value binding) val frame))
          ((var? val)                      ; ***
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match
                  var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame)     ; ***
           'failed)
          (else (extend var val frame)))))

(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let ((b (binding-in-frame e frame)))
                 (if b
                     (tree-walk (binding-value b))
                     false))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))

;;; answer
;;; improved and
(define (new-conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (merge-frame-stream
        (qeval (first-conjunct conjuncts)
               frame-stream)
        (new-conjoin (rest-conjuncts conjuncts)
                     frame-stream))))
(put 'and 'qeval new-conjoin)

(define (merge-frame-stream s1 s2) 
    (cond ((stream-null? s1) s2) 
          ((stream-null? s2) s1) 
          (else 
              (stream-flatmap 
                  (lambda (frame1) 
                      (stream-flatmap 
                          (lambda (frame2) 
                              (merge-if-compatible frame1 frame2))
                              s2)) 
                  s1))))

(define (merge-if-compatible frame1 frame2)
    (if (eq? frame2 'failed) 
        the-empty-stream
        (if (null? frame1)
            (singleton-stream frame2)
            (let* ((binding1 (car frame1))
                   (var1 (binding-variable binding1))
                   (val1 (binding-value binding1)))
                (merge-if-compatible
                    (cdr frame1)
                    (extend-if-possible var1 val1 frame2))))))
;;; test
(load "/home/lancecopper/code/SICP/logic-interpreter.scm")

;;; copy answer into interpreter

(query-driver-loop)
(assert! (age lance 27))
;;; copy job-assert!.scm



(and (job ?person (computer programmer))
     (address ?person ?where))


;;; a
(and (supervisor ?person (Bitdiddle Ben))
     (address ?person ?where))

;;; b
(and (salary ?person ?amount1)
     (salary (Bitdiddle Ben) ?amount2)
     (lisp-value < ?amount1 ?amount2))

;;; c
(and (supervisor ?person1 ?person2)
     (not (job person2 (computer . ?type))))



(assert! (age lance 27))

(and (age ?who ?age)
     (lisp-value < ?age 30))



