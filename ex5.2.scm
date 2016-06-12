

(data-paths
 (registers
  ((name a)
   (buttons ((name a<-b) (source (register b)))))
  ((name b)
   (buttons ((name b<-t) (source (register t)))))
  ((name t)
   (buttons ((name t<-r) (source (operation rem))))))

 (operations
  ((name rem)
   (inputs (register a) (register b)))
  ((name =)
   (inputs (register b) (constant 0)))))


(controller
 test-b                           ; label
   (test =)                       ; test
   (branch (label gcd-done))      ; conditional branch
   (t<-r)                         ; button push
   (a<-b)                         ; button push
   (b<-t)                         ; button push
   (goto (label test-b))          ; unconditional branch
 gcd-done)                        ; label



(controller
 test-b
   (test (op =) (reg b) (const 0))
   (branch (label gcd-done))
   (assign t (op rem) (reg a) (reg b))
   (assign a (reg b))
   (assign b (reg t))
   (goto (label test-b))
 gcd-done)



(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))


;;; answer

(controller
 test-c
    (test (op >) (reg c) (reg n))
    (branch (label fact-done))
    (assign t1 (op *) (reg p) (reg c))
    (assign t2 (op +) (reg c) (const 1))
    (assign p (reg t1))
    (assign c (reg t2))
    (goto (label test-c))
 fact-done)



