; CPS factorial function
(define (fact n)
  (fact/k n (end-cont)))

(define (fact/k n cont)
  (if (zero? n)
    (apply-cont cont 1)
    (fact/k (- n 1) (fact1-cont n cont))))

(define (end-cont)
  (lambda (val) val))

(define (fact1-cont n saved-cont)
  (lambda (val) 
    (apply-cont saved-cont (* n val))))

(define (apply-cont cont val)
  (cont val))
