  
(define (lex-address addr-frame addr-offset) 
  (list addr-frame addr-offset)) 

(define (addr-frame lex-address)
    (car lex-address))

(define (addr-offset lex-address)
    (cadr lex-address))

(define (lexical-address-lookup env address) 
    (let* ((frame (list-ref env (addr-frame address))) 
           (value (list-ref (frame-values frame) (addr-offset address)))) 
        (if (eq? value '*unassigned*) 
                (error "the variable is unassigned -- LEXICAL-ADDRESS-LOOKUP" address) 
                value)))

(define (lexical-address-set! address env value) 
    (let ((frame (list-ref env (addr-frame address)))
          (offset (addr-offset address)))
        (define (set-value! f pos) 
            (if (= f 0) 
                    (set-car! f value) 
                    (set-value! (cdr f (- pos 1))))) 
            (set-value! frame offset value)))

;;; 