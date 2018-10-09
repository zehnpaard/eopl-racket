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
    (diff-exp (exp1 exp2)
      (value-of/k exp1 env (diff1-cont exp2 env cont)))
    (call-exp (rator rand)
      (value-of/k rator env (rator-cont rand env cont)))
    ))

(define (apply-procedure/k proc1 val cont)
  (cases proc proc1
    (procedure (var body saved-env)
      (value-of/k body (extend-env var val saved-env) cont))))

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

(define (diff1-cont exp2 env cont)
  (lambda (val)
    (value-of/k exp2 env (diff2-cont val cont))))

(define (diff2-cont val1 cont)
  (lambda (val2)
    (apply-cont cont
      (num-val
        (- (expval->num val1)
           (expval->num val2))))))

(define (rator-cont rand env cont)
  (lambda (rator)
    (value-of/k rand env (rand-cont rator cont))))

(define (rand-cont rator cont)
  (lambda (rand)
    (apply-procedure/k rator rand cont)))

(define (apply-cont cont v)
  (cont v))
