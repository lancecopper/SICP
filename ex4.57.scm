
(rule (can-replace ?person1 ?person2)
      (and (job ?person1 ?type1)
           (job ?person2 ?type2)
           (or (same? type1 type2)
               (can-do-job ?type1 ?type2))))

;;; a
(can-replace ?person1 (Fect Cy D))

;;; b
(and (salary ?person1 ?amount1)
     (salary ?person2 ?amount2)
     (can-replace ?person1 ?person2)
     (lisp-value < ?amount1 ?amount2))




