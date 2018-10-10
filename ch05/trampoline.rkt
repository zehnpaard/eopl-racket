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

(define (apply-cont cont val)
  (cases continuation cont
    (end-cont ()
      (begin
        (eopl:printf "End of computation.~%")
        val))
    (zero1-cont (saved-cont)
      (apply-cont saved-cont
        (bool-val
          (zero? (expval->num val)))))
    (let-exp-cont (var body saved-env saved-cont)
      (value-of/k body (extend-env var val saved-env) saved-cont))
    (if-test-cont (exp2 exp3 saved-env saved-cont)
      (if (expval->bool val)
        (value-of/k exp2 saved-env saved-cont)
        (value-of/k exp3 saved-env saved-cont)))))
