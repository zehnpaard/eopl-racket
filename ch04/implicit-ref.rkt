(define (value-of e env1)
  (cases expression e
    (const-exp (n)
      (num-val n))
    (var-exp (v)
      (deref (apply-env env1 v)))
    (zero?-exp (e)
      (bool-val (zero? (expval->num (value-of e env1)))))
    (diff-exp (e1 e2)
      (num-val (- (expval->num (value-of e1 env1))
                  (expval->num (value-of e2 env1)))))
    (if-exp (cond-e true-e false-e)
      (if (expval->bool (value-of cond-e env1))
        (value-of true-e env1)
        (value-of false-e env1)))
    (let-exp (v e1 e2)
      (value-of e2 (extend-env v (value-of e1 env1) env1)))
    (proc-exp (v e1)
      (proc-val (procedure v e1 env1)))
    (call-exp (e1 e2)
      (apply-procedure
       (expval->proc (value-of e1 env1))
       (value-of e2 env1)))
    ))

; store
(define (empty-store)
  '())

(define the-store 'uninitialized)

(define (get-store)
  the-store)

(define (initialize-store!)
  (set! the-store (empty-store)))

(define (reference? v)
  (integer? v))

(define (newref val)
  (set! the-store (append the-store (list val)))
  (- (length the-store) 1))

(define (deref ref)
  (list-ref the-store ref))

(define (setref! ref val)
  (define (setref-inner store1 ref1)
    (cond
      ((null? store1)
       (eopl:error 'setref-inner "Invalid reference ~s" ref1))
      ((zero? ref1)
       (cons val (cdr store1)))
      (else
       (cons
        (car store1)
        (setref-inner (cdr store1) (- ref1 1))))))
  (set! the-store (setref-inner the-store ref)))


