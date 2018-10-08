(define (value-of-program pgm)
  (cases program pgm
    (a-program (exp1)
      (value-of/k exp1 (init-env) (end-cont)))))

(define (value-of/k exp env cont)
  (cases expression exp
    (const-exp (num)
      (apply-cont cont (num-val num)))
    (var-exp (var)
      (apply-cont cont (apply-env env var)))
    (proc-exp (var body)
      (apply-cont cont
        (proc-val (procedure var body env))))
    (letrec-exp (p-name b-var p-body letrec-body)
      (value-of/k letrec-body (extend-env-rec p-name b-var p-body env) cont))
    (zero?-exp (exp1)
      (value-of/k exp1 env (zero1-cont cont)))
    (let-exp (var exp1 body)
      (value-of/k exp1 env (let-exp-cont var body env cont)))
    (if-exp (exp1 exp2 exp3)
      (value-of/k exp1 env (if-test-cont exp2 exp3 env cont)))
    ))

(define (apply-procedure proc1 val)
  (cases proc proc1
    (procedure (var body saved-env)
      (value-of body (extend-env var val saved-env)))))

(define (end-cont)
  (lambda (val)
    (begin
      (eopl:printf "End of computation.~%")
      val)))

(define (zero1-cont cont)
  (lambda (val)
    (apply-cont cont
      (bool-val
        (zero? (expval->num val))))))

(define (let-exp-cont var body env cont)
  (lambda (val)
    (value-of/k body (extend-env var val env) cont)))

(define (if-test-cont exp2 exp3 env cont)
  (lambda (val)
    (if (expval->bool val)
      (value-of/k exp2 env cont)
      (value-of/k exp3 env cont))))

(define (apply-cont cont v)
  (cont v))
