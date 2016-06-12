(son Adam Cain)
(son Cain Enoch)
(son Enoch Irad)
(son Irad Mehujael)
(son Mehujael Methushael)
(son Methushael Lamech)
(wife Lamech Ada)
(son Ada Jabal)
(son Ada Jubal)


;;; 
(rule (grandson ?g ?s)
      (and (son ?g ?f)
           (son ?f s)))


(rule (son ?m ?s)
      (and (son ?w ?s)
           (wife ?m ?w)))
























