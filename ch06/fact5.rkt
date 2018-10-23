; CPS factorial function
(define (fact n)
  (fact/k n
          (lambda (val) val)))

(define (fact/k n cont)
  (if (zero? n)
    (cont 1)
    (fact/k (- n 1)
            (lambda (val)
              (cont (* n val))))))

