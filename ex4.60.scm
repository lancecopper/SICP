
(lives-near ?person (Hacker Alyssa P))

(lives-near ?person-1 ?person-2)



(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2))))



















