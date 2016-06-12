
(and (can-do-job ?x (computer programmer trainee))
     (job ?person ?x))

(lives-near ?x (Hacker Alyssa P))

;;; old
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss))))

;;; new(which get into infinite loop)
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by ?middle-manager ?boss)
               (supervisor ?staff-person ?middle-manager)))


































