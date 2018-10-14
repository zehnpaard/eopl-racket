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
    (try-exp (exp1 var handler-exp)
      (value-of/k exp1 env (try-cont var handler-exp env cont)))
    (raise-exp (exp1)
      (value-of/k exp1 env (raise1-cont cont)))
    ))

(define (apply-procedure/k proc1 arg1 cont)
  (cases proc proc1
    (procedure (var body saved-env)
      (value-of/k body (extend-env var arg1 saved-env) cont))))

(define-datatype continuation continuation?
  (end-cont)
  (zero1-cont
    (cont continuation?))
  (let-exp-cont
    (var identifier?)
    (body expression?)
    (env environment?)
    (cont continuation?))
  (if-test-cont
    (exp2 expression?)
    (exp3 expression?)
    (env environment?)
    (cont continuation?))
  (diff1-cont
    (exp2 expression?)
    (env environment?)
    (cont continuation?))
  (diff2-cont
    (val1 expval?)
    (cont continuation?))
  (rator-cont
    (rand expression?)
    (env environment?)
    (cont continuation?))
  (rand-cont
    (val1 expval?)
    (cont continuation?))
  (try-cont
    (var identifier?)
    (handler-exp expression?)
    (env environment?)
    (cont continuation?))
  (raise1-cont
    (saved-cont continuation?))
  )

(define (apply-cont cont val)
  (cases continuation cont
    (end-cont ()
      (begin
        (eopl:printf "End of computation.~%")
        val))
    (zero1-cont (saved-cont)
      (apply-cont saved-cont
        (bool-val
          (zero? (expval->num val)))))
    (let-exp-cont (var body saved-env saved-cont)
      (value-of/k body (extend-env var val saved-env) saved-cont))
    (if-test-cont (exp2 exp3 saved-env saved-cont)
      (if (expval->bool val)
        (value-of/k exp2 saved-env saved-cont)
        (value-of/k exp3 saved-env saved-cont)))
    (diff1-cont (exp2 saved-env saved-cont)
      (value-of/k exp2 saved-env (diff2-cont val saved-cont)))
    (diff2-cont (val1 saved-cont)
      (apply-cont saved-cont
        (num-val (- (expval->num val1)
                    (expval->num val)))))
    (rator-cont (rand saved-env saved-cont)
      (value-of/k rand saved-env (rand-cont val saved-cont)))
    (rand-cont (val1 saved-cont)
      (apply-procedure/k (expval->proc val1) val saved-cont))
    (try-cont (var handler-exp saved-env saved-cont)
      (apply-cont saved-cont val))
    (raise1-cont (saved-cont)
      (apply-handler val saved-cont))
    ))

