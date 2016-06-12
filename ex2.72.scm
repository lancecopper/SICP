
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))



(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))


(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))



(define (encode-symbol x tree)
    (cond ((and (leaf? tree) (memq x (symbols tree))) 
            '())
          ((not (memq x (symbols tree))) 
            (error "symbol not found in tree----" x))
          ((memq x (symbols (left-branch tree)))
            (cons 0 (encode-symbol x (left-branch tree))))
          ((memq x (symbols (right-branch tree)))
            (cons 1 (encode-symbol x (right-branch tree))))
          (else (error "ill-formed tree"))))













