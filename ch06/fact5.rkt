; CPS factorial function
(define (fact n)
  (fact/k n
          (lambda (val) val)))

(define (fact/k n cont)
  (if (zero? n)
    (apply-cont cont 1)
    (fact/k (- n 1)
            (lambda (val)
              (apply-cont cont (* n val))))))

(define (apply-cont cont val)
  (cont val))
