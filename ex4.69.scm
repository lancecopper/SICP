
(son Adam Cain)
(son Cain Enoch)
(son Enoch Irad)
(son Irad Mehujael)
(son Mehujael Methushael)
(son Methushael Lamech)
(wife Lamech Ada)
(son Ada Jabal)
(son Ada Jubal)


(rule (grandson ?g ?s)
      (and (son ?g ?f)
           (son ?f s)))


(rule (son ?m ?s)
      (and (son ?w ?s)
           (wife ?m ?w)))


(rule (last-pair (?x) ?x))
(rule (last-pair (?y . ?z) ?x)
      (last-pair ?z ?x))

;;;
(rule (ends-in-grandson ?rel)
      (last-pair ?rel grandson))


(rule ((great . ?rel) ?x ?y)
      (and
        (ends-in-grandson ?rel)
        (son ?x ?z)
        (or (?rel ?z ?y)
            (and (same ?rel (?rel-car ()))
                 (rel-car ?z ?y)))))







