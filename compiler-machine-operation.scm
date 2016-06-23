
(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))

(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))

(define (compiled-procedure-entry c-proc) (cadr c-proc))

(define (compiled-procedure-env c-proc) (caddr c-proc))

(define label-counter 0)

(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(define (make-label name)
  (string->symbol
    (string-append (symbol->string name)
                   (number->string (new-label-number)))))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

(define all-regs '(env proc val argl continue))

(define (registers-needed s)
  (if (symbol? s) '() (car s)))
(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))
(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))
(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))


(define eceval-operations
    (append eceval-operations
        (list
            (list 'make-compiled-procedure make-compiled-procedure)
            (list 'compiled-procedure? compiled-procedure?)
            (list 'compiled-procedure-entry compiled-procedure-entry)
            (list 'compiled-procedure-env compiled-procedure-env)
            (list 'new-label-number new-label-number)
            (list 'make-label make-label)
            (list 'empty-instruction-sequence empty-instruction-sequence)
            (list 'registers-needed registers-needed)
            (list 'registers-modified registers-modified)
            (list 'statements statements)
            (list 'needs-register? needs-register?)
            (list 'modifies-register? modifies-register?))))


;;; added for ex5.38
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

(define eceval-operations
    (append eceval-operations
        (list
            (list 'get-global-environment get-global-environment)
            (list 'lexical-address-lookup lexical-address-lookup)
            (list 'lexical-address-set! lexical-address-set!))))


