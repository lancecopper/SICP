
(rule (append-to-form () ?y ?y))
(rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z))

(rule (last-pair (?x) ?x))
(rule (last-pair (?y . ?z) ?x)
      (last-pair ?z ?x))


(reverse (1 2 3) ?x)
(reverse ?x (1 2 3))

;;; implementation the rule below needed loop detector of ex4.67
(rule (reverse (?x) (?x)))
(rule (reverse (?car1 . ?cdr1) (?car2 . ?cdr2))
      (or
          (and
              (reverse ?cdr1 ?cdr1-rev)
              (append-to-form ?cdr1-rev ?car1 ?num2)
              (append-to-form ?car2 ?cdr2 ?num2))
          (and
              (reverse ?cdr2 ?cdr2-rev)
              (append-to-form ?cdr2-rev ?car2 ?num1)
              (append-to-form ?car1 ?cdr1 ?num1))))








