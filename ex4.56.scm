

(and (job ?person (computer programmer))
     (address ?person ?where))


;;; a
(and (supervisor ?person (Bitdiddle Ben))
     (address ?person ?where))

;;; b
(and (salary ?person ?amount1)
     (salary (Bitdiddle Ben) ?amount2)
     (lisp-value < ?amount ?amount2))

;;; c
(and (supervisor ?person1 ?person2)
     (not (job ?person2 (computer . ?type))))








