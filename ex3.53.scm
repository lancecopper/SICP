
(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))



(define s (cons-stream 1 (add-streams s s)))
(stream-ref s 1)
(stream-ref s 2)
(stream-ref s 3)




