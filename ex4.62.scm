(rule (?x next-to ?y in (?x ?y . ?u)))

(rule (?x next-to ?y in (?v . ?z))
      (?x next-to ?y in ?z))


(rule (append-to-form () ?y ?y))
(rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z))



(rule (last-pair (?x) ?x))
(rule (last-pair (?y . ?z) ?x)
      (last-pair ?z ?x))























