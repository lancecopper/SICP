

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

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))



(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


(define (successive-merge leaves) 
   (if (null? (cdr leaves)) 
       (car leaves) 
       (successive-merge 
        (adjoin-set (make-code-tree (car leaves) (cadr leaves)) 
                    (cddr leaves))))) 

(define rock-pairs '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))

(define rock-tree (generate-huffman-tree rock-pairs))

(encode '(Get a job) rock-tree)

(encode '(Get a job

Sha na na na na na na na na

Get a job

Sha na na na na na na na na

Wah yip yip yip yip yip yip yip yip yip

Sha boom) rock-tree)









