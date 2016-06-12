; point-related function

(define (make-point x y)
    (cons x y))

(define (x-point x)
    (car x))

(define (y-point x)
    (cdr x))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (points-distance point1 point2)
    (sqrt (+ (square (- (x-point point1) (x-point point2)))
             (square (- (y-point point1) (y-point point2)))
          )))

; express rectangular with 4 points: lu, ld, ru, rd.

(define (make-rectangular lu ld ru rd)
    (list lu ld ru rd))

(define (get-item n get-list)
    (if (= n 1) (car get-list)
        (get-item (- n 1) (cdr get-list))))

(define (get-lu rec)
    (get-item 1 rec))

(define (get-ld rec)
    (get-item 2 rec))

(define (get-ru rec)
    (get-item 3 rec))

(define (get-rd rec)
    (get-item 4 rec))


(define rec1
    (make-rectangular (make-point 0 1) 
                      (make-point 0 0)
                      (make-point 1 1)
                      (make-point 1 0)))



; express rectangular with 2 points: lu, rd.

(define (make-rectangular lu rd)
    (cons lu rd))

(define (get-lu rec)
    (car rec))

(define (get-rd rec)
    (cdr rec))

(define (get-ld rec)
    (make-point (x-point (get-lu rec)) (y-point (get-rd rec))))

(define (get-ru rec)
    (make-point (x-point (get-rd rec)) (y-point (get-lu rec))))

(define rec2
    (make-rectangular (make-point 0 1) 
                      (make-point 1 0)))

; calc length, width, perimeter, area

(define (get-length-rec rec)
    (points-distance (get-lu rec) (get-ld rec)))

(define (get-width-rec rec)
    (points-distance (get-lu rec) (get-ru rec)))

(define (rec-perimeter rec)
    (* (+ (get-length-rec rec) (get-width-rec rec)) 2))

(define (rec-area rec)
    (* (get-length-rec rec) (get-width-rec rec)))

