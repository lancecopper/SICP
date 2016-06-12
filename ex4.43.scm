;;;
Mr.Moore                    Lorna               Mary
Colonel Downing             Melissa            
Mr. Hall                    Rosalind            
Sir Barnacle Hood           Gabrielle           Melissa
Dr. Parker                  Mary                'x
z                            'x                  Gabrielle


;;;
(define (father-daughter)
  (let ((Moore  'Mary)
        (Downing (amb 'Gabrielle 'Lorna 'Rosalind))
        (Hall (amb 'Gabrielle 'Lorna))
        (Hood 'Melissa)
        (Parker (amb 'Lorna 'Rosalind)))
        (require 
            (cond
                ((eq? Downing 'Gabrielle) (eq? Melissa Parker))
                ((eq? Hall 'Gabrielle) (eq? 'Rosalind Parker))
                (else false)))
        (require (distinct? (list Hall Downing Parker)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))
