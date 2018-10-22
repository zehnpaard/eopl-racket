; Non-tail-recursive, non-CPS factorial function
(define (fact n)
  (if (zero? n)
    1
    (* n (fact (- n 1)))))
