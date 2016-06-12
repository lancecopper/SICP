(define (for-each proc items)
    (if (null? items)
        "okay"
        (begin (proc (car items)) (for-each proc (cdr items)))))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (make-vect x y)
    (cons x y))

(define (xcor-vect vector)
    (car vector))

(define (ycor-vect vector)
    (cdr vector))

(define (add-vect v1 v2)
    (make-vect (+ (xcor-vect v1) (xcor-vect v2)) 
               (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
    (make-vect (- (xcor-vect v1) (xcor-vect v2)) 
               (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect scale vector)
    (make-vect (* scale (xcor-vect vector)) 
               (* scale (ycor-vect vector))))

(define (make-segment start-vector end-vector)
    (cons start-vector end-vector))

(define (start-segment segment)
    (car segment))

(define (end-segment segment)
    (cdr segment))


(define (origin-frame frame)
    (car frame))

(define (edge1-frame frame)
    (car (cdr frame)))

(define (edge2-frame frame)
    (cdr (cdr frame)))

;;; <a>
(define (outline frame)
    (let ((segment-list (list 
                            (make-segment (origin-frame frame) (edge1-frame frame))
                            (make-segment (origin-frame frame) (edge2-frame frame))
                            (make-segment (edge1-frame frame) (add-vect (edge1-frame) (edge2-frame)))
                            (make-segment (edge2-frame) (add-vect (edge1-frame) (edge2-frame)))
                        )))
        ((segments->painter segment-list) frame)))

;;; <b>
(define (xline frame)
    (let ((segment-list (list 
                            (make-segment (origin-frame frame) (add-vect (edge1-frame) (edge2-frame)))
                            (make-segment (edge1-frame frame) (edge2-frame frame))
                        )))
        ((segments->painter segment-list) frame)))

;;; <c>
(define (xline frame)
    (let ((midp1 (scale-vect 0.5 (edge1-frame frame)))
          (midp2 (add-vect (edge1-frame frame) (scale-vect 0.5 (edge2-frame frame))))
          (midp3 (scale-vect 0.5 (edge2-frame frame)))
          (midp4 (add-vect (edge2-frame frame) (scale-vect 0.5 (edge1-frame frame)))))
         ((let ((segment-list (list 
                            (make-segment midp1 midp2)
                            (make-segment midp2 midp3)
                            (make-segment midp3 midp4)
                            (make-segment midp4 midp1))
                        )))
        ((segments->painter segment-list) frame))))

    
(define (wave frame)
    ())







