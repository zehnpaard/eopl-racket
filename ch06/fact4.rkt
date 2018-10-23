(define n 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)
(define pc 'uninitialized)

(define (fact arg-n)
  (set! cont (end-cont))
  (set! n arg-n)
  (set! pc fact/k)
  (trampoline!)
  val)

(define (trampoline!)
  (if pc
    (begin
      (pc)
      (trampoline!))))

(define (fact/k)
  (if (zero? n)
    (begin
      (set! val 1)
      (set! pc apply-cont))
    (begin
      (set! cont (fact1-cont n cont))
      (set! n (- n 1))
      (set! pc fact/k))))

(define (apply-cont)
  (cases continuation cont
    (end-cont ()
      (set! pc #f))
    (fact1-cont (saved-n saved-cont)
      (set! cont saved-cont)
      (set! n saved-n)
      (set! pc apply-cont))))

(define-datatype continuation continuation?
  (end-cont)
  (fact1-cont
    (n integer?)
    (cont continuation?)))
