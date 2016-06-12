(load "/home/lancecopper/code/SICP/logic-interpreter.scm")

(query-driver-loop)

(assert! (married Minnie Mickey))

(married Minnie ?who)

(unique (married Minnie ?who))

(unique (job ?x (computer wizard)))

(job ?x (computer wizard))

(job (Bitdiddle Ben) ?y)



;; add those code 
(define (unique-query exps) (car exps))

(define (uniquely-asserted pattern frame-stream) 
 (stream-flatmap 
  (lambda (frame) 
   (let ((stream (qeval (unique-query pattern) 
                                        (singleton-stream frame)))) 
        (if (singleton-stream? stream) 
            stream 
            the-empty-stream))) 
  frame-stream)) 

(put 'unique 'qeval uniquely-asserted) 


(define (singleton-stream? s) 
 (and (not (stream-null? s)) 
      (stream-null? (stream-cdr s)))) 


















