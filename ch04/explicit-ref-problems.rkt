#lang eopl

;4.6
(define (value-of e env1)
  (cases expression e
    (setref-exp (exp1 exp2)
      (let ((val2 (value-of exp2 env1)))
        (begin
          (setref! (expval->ref (value-of exp1 env1))
                   val2)
          val2)))))