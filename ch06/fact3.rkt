(define n 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)

(define-datatype continuation continuation?
  (end-cont)
  (fact1-cont
    (n integer?)
    (cont continuation?)))

