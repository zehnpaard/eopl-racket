(define (fib n)
  (fib/k n (end-cont)))

(define (fib/k n cont)
  (if (< n 2)
    (apply-cont cont 1)
    (fib/k (- n 1) (fib1-cont n cont))))

(define-datatype continuation continuation?
  (empty-cont)
  (fib1-cont (n saved-cont))
  (fib2-cont (v saved-cont)))

(define (apply-cont cont val)
  (cases continuation cont
    (empty-cont () val)
    (fib1-cont (n saved-cont)
      (fib/k (- n 2) (fib2-cont val saved-cont)))
    (fib2-cont (v saved-cont)
      (apply-cont saved-cont (+ v val)))))
