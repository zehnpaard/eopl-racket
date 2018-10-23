; CPS factorial function
(define (fact n)
  (fact/k n (end-cont)))

(define (fact/k n cont)
  (if (zero? n)
    (apply-cont cont 1)
    (fact/k (- n 1) (fact1-cont n cont))))

(define-datatype continuation continuation?
  (end-cont)
  (fact1-cont
    (n integer?)
    (cont continuation?)))

(define (apply-cont cont val)
  (cases continuation cont
    (end-cont ()
      val)
    (fact1-cont (n saved-cont)
      (apply-cont saved-cont (* n val)))))
