
start
  (goto (label here))
here
  (assign a (const 3))
  (goto (label there))
here
  (assign a (const 4))
  (goto (label there))
there


;;; a
a=3
;;; reason
;;; labels will be 
;;;     ((here (assign a (const 3))...),
;;;      (here (assign a (const 4))...))

;;; b

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text))
               (label-names '()))
           (if (symbol? next-inst)
               (if (assoc next-inst label-names)
                   (error "duplicate label name----ASSEMBLE" next-inst)
                   (receive insts
                            (cons (make-label-entry next-inst
                                                    insts)
                                  labels)))
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels)))))))
