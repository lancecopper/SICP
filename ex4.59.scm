

(meeting accounting (Monday 9am))
(meeting administration (Monday 10am))
(meeting computer (Wednesday 3pm))
(meeting administration (Friday 1pm))

(meeting whole-company (Wednesday 4pm))

;;; a
(meeting ?division (Friday ?time))

;;; b
(rule (meeting-time ?person ?day-and-time)
      (and (job ?person (?division ?position))
           (meeting ?division ?day-and-time)))

;;; c

(and (meeting-time (Hacker Alyssa P) ?day-and-time)
     (same ?day-and-time (Wednesday ?time))
     (meeting ?division ?day-and-time)
     (job (Hacker Alyssa P) (?division ?position)))


qeval:
and
or
not
lisp-value
simple query
assert








