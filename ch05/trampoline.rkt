(define (value-of-program pgm)
  (cases program pgm
    (a-program (exp)
      (trampoline
        (value-of/k exp (init-env) (end-cont))))))

(define (trampoline bounce)
  (if (expval? bounce)
    bounce
    (trampoline (bounce))))
