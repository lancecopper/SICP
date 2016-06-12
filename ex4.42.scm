(load "/home/lancecopper/code/SICP/ex4.41.scm")

(define (either x y)
    (or (and x (not y))
        (and (not x) y)))

(define (multiple-ranking)
    (define (iter-arrange list-n)
        (flatmap 
            (lambda 
                (x) 
                (let ((next (remove x list-n)))
                     (if (null? next)
                         (list (list x))
                         (map 
                            (lambda (p) (cons x p)) 
                            (iter-arrange next)))))
            list-n))
    (define ori-multiple-dwelling (iter-arrange (list 1 2 3 4 5)))
    (define (invalid-solution permutation)
        (let ((Betty (first permutation))
              (Ethel (second permutation))
              (Joan (third permutation))
              (Kitty (fourth permutation))
              (Mary (fifth permutation)))
              (and
                  (either (= Kitty 2) (= Betty 3))
                  (either (= Ethel 1) (= Joan 2))
                  (either (= Joan 3) (= Ethel 5))
                  (either (= Kitty 2) (= Mary 4))
                  (either (= Mary 4) (= Betty 1)))))
    (define (present-solution solution)
        (map
            (lambda (x)
                (map list 
                     '(Betty Ethel Joan Kitty Mary)
                     x))
            solution))
    (present-solution
        (filter invalid-solution ori-multiple-dwelling)))







