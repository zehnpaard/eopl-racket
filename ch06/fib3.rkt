(define (fib n)
  (fib/k n (end-cont)))

(define (fib/k n cont)
  (if (< n 2)
    (apply-cont cont 1)
    (fib/k (- n 1) (fib1-cont n cont))))

(define (empty-cont)
  (lambda (val) val))

(define (fib1-cont n saved-cont)
  (lambda (val)
    (fib/k (- n 2) (fib2-cont val saved-cont))))

(define (fib2-cont v saved-cont)
  (lambda (val)
    (apply-cont saved-cont (+ v val))))

(define (apply-cont cont val)
  (cont val))
