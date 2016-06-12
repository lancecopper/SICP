(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(dot-product (list 1 2 3) (list 4 5 6))


(define matrix (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))


(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(matrix-*-vector matrix (list 1 1 1 1))


(define (transpose mat)
  (accumulate-n cons nil mat))


(define nil '())

(transpose matrix)


(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(define matrix1 (list (list 1 4 6) (list 2 5 7) (list 3 6 8) (list 4 6 9)))

(matrix-*-matrix matrix matrix1)
(matrix-*-matrix matrix1 matrix)

