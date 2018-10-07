(define (value-of-program pgm)
  (cases program pgm
    (a-program (exp1)
      (value-of exp1 (init-env)))))

(define (value-of exp env)
  (cases expression exp
    (const-exp (num) (num-bal num))
    (var-exp (var) (apply-env env var))
    (diff-exp (exp1 exp2)
      (let ((num1 (expval->num (value-of exp1 env)))
            (num2 (expval->num (value-of exp2 env))))
        (num-val (- num1 num2))))
    (zero?-exp (exp1)
      (let ((num1 (expval->num (value-of exp1 env))))
        (if (zero? num1) (bool-val #t) (bool-val #f))))
    (if-exp (exp1 exp2 exp3)
      (if (expval->bool (value-of exp1 env))
        (value-of exp2 env)
        (value-of exp3 env)))
    (let-exp (var exp1 exp2)
      (let ((val1 (value-of exp1 env)))
        (value-of body (extend-env var val1 env))))
    (proc-exp (var body)
      (proc-val (procedure var body env)))
    (call-exp (rator rand)
      (let ((proc1 (expval-Lproc (value-of rator env)))
            (arg (value-of rand env)))))
    (letrec-exp (p-name b-var p-body letrec-body)
      (value-of letrec-body
        (extend-env-rec p-name b-var p-body env)))
    ))

(define (apply-procedure proc1 val)
  (cases proc proc1
    (procedure (var body saved-env)
      (value-of body (extend-env var val saved-env)))))

