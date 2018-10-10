(define (value-of-program pgm)
  (cases program pgm
    (a-program (exp)
      (trampoline
        (value-of/k exp (init-env) (end-cont))))))

(define (trampoline bounce)
  (if (expval? bounce)
    bounce
    (trampoline (bounce))))

(define-datatype continuation continuation?
  (end-cont)
  (zero1-cont
    (cont continuation?))
  (let-exp-cont
    (var identifier?)
    (body expression?)
    (env environment?)
    (cont continuation?))
  (if-test-cont
    (exp2 expression?)
    (exp3 expression?)
    (env environment?)
    (cont continuation?)))
