

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))


(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))



How would (parse ‘(the cat studies with the student)) be evaluated if evaluation of arguments is right to left?

    (parse '(the cat studies with the student)) 
(set! *unparsed* '(the cat studies with the student))
(let ((sent (parse-sentence))
    (parse-sentence) 
(list 'sentence (parse-noun-phrase) (parse-verb-phrase))))
    (parse-verb-phrase) 
(maybe-extend (parse-word verbs))
    (parse-word '(verb studies lectures eats sleeps))
(require (not (null? *unparsed*))) 
(require (memq (car *unparsed*) (cdr '(verb studies lectures eats sleeps))))
(cdr '(verb studies lectures eats sleeps)) => '(studies lectures eats sleeps)
(car *unparsed*) => 'the
fails


