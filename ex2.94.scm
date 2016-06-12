


;;; 下述方案仅仅处理了当term-list都是scheme-number时的问题，当然处理rational, real和 complex思路与此一致。
;;; 但是，当term-list含有polymonial时情况变得复杂，因为,这意味着不能仅仅通过设为order　0来转化。



(define p1 (make-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
(define p2 (make-polynomial 'x '((3 1) (1 -1))))
(greatest-common-divisor p1 p2)



(define (gcd-poly p1 p2) 
  (if (same-varaible? (variable p1) (variable p2)) 
        (make-poly (variable p1) 
                   (gcd-terms (term-list p1) 
                              (term-list p2)) 
        (error "not the same variable -- GCD-POLY" (list p1 p2)))))



(put 'greatest-common-divisor '(polynomial polynomial) 
         (lambda (a b) (tag (gcd-poly a b))))



