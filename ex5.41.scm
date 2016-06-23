(define (find-variable var ct-env)
    (let ((addr-frame 0)
          (addr-offset 0))
        (define (loop-env ct-env)
            (define (scan ct-frame)
                (if (null? ct-frame)
                    (begin
                        (set! addr-frame (+ addr-frame 1))
                        (set! addr-offset 0)
                        (loop-env (cdr ct-env)))
                    (if (eq? var (car ct-frame))
                        (list addr-frame addr-offset)
                        (begin
                            (set! addr-offset (+ addr-offset 1))
                            (scan (cdr ct-frame))))))
            (if (null? ct-env)
                'not-found
                (scan (car ct-env))))
        (loop-env ct-env)))


(find-variable 'c '((y z) (a b c d e) (x y)))

(find-variable 'x '((y z) (a b c d e) (x y)))

(find-variable 'w '((y z) (a b c d e) (x y)))
;not-found


