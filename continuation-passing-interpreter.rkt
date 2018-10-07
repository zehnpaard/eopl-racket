(define (value-of-program pgm)
  (cases program pgm
    (a-program (exp1)
      (value-of exp1 (init-env)))))

(define (value-of exp env)
  (cases expression exp
    (const-exp (num) (num-bal num))
    (var-exp (var) (apply-env env var))
    (diff-exp (exp1 exp2)
      (num-val
        (- (expval->num (value-of exp1 env))
           (expval->num (value-of exp2 env)))))
    (zero?-exp (exp1)
      (bool-val 
        (zero? (expval->num (value-of exp1 env)))))
    (if-exp (exp1 exp2 exp3)
      (if (expval->bool (value-of exp1 env))
        (value-of exp2 env)
        (value-of exp3 env)))
    (let-exp (var exp1 exp2)
      (value-of body (extend-env var (value-of exp1 env) env)))
    (proc-exp (var body)
      (proc-val (procedure var body env)))
    (call-exp (rator rand)
      (apply-procedure
        (expval->proc (value-of rator env))
        (value-of rand env)))
    (letrec-exp (p-name b-var p-body letrec-body)
      (value-of letrec-body
        (extend-env-rec p-name b-var p-body env)))
    ))

(define (apply-procedure proc1 val)
  (cases proc proc1
    (procedure (var body saved-env)
      (value-of body (extend-env var val saved-env)))))

