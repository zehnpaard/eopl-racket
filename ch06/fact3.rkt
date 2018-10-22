(define n 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)

(define-datatype continuation continuation?
  (end-cont)
  (fact1-cont
    (n integer?)
    (cont continuation?)))

(define (fact m)
  (set! cont (end-cont))
  (set! n m)
  (fact/k))

(define (fact/k)
  (if (zero? n)
    (begin
      (set! val 1)
      (apply-cont))
    (begin
      (set! cont (fact1-cont n cont))
      (set! n (- n 1))
      (fact/k))))
