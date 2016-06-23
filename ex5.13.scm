((machine 'allocate-register) register-name)

(define (make-new-machine)
    ... ...
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (begin
                ((machine 'allocate-register) name)
                (lookup-register name))
              )))
      ... ...)


(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)    
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))





