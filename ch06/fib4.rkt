(define (fib n)
  (fib/k n (lambda (val) val)))

(define (fib/k n cont)
  (if (< n 2)
    (apply-cont cont 1)
    (fib/k (- n 1)
           (lambda (val)
             (fib/k (- n 1)
                    (lambda (val)
                      (apply-cont cont (+ v val))))))))

(define (apply-cont cont val)
  (cont val))
