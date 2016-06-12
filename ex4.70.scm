(load "/home/lancecopper/code/SICP/logic-interpreter.scm")

;;; answer
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (set! THE-ASSERTIONS
        (cons-stream assertion THE-ASSERTIONS))
  'ok)





























































