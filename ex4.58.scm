
(rule (big-shot ?person1)
      (and (job ?person1 (?division ?position1))
          (not 
            (and (supervisor ?person1 ?person2)
                 (job ?person2 (?division ?position2))))))








