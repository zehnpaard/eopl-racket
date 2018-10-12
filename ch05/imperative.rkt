(define exp 'uninitialized)
(define env 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)
(define proc1 'uninitialized)

(define-datetype continuation continuation?
  (end-cont)
  (zero1-cont
    (saved-cont continuation?))
  (let-exp-cont
    (var identifier?)
    (body expression?)
    (saved-env environment?)
    (saved-cont continuation?))
  (if-test-cont
    (exp2 expression?)
    (exp3 expression?)
    (saved-env environment?)
    (saved-cont continuation?))
  (diff1-cont
    (exp2 expression?)
    (saved-env environment?)
    (saved-cont continuation?))
  (diff2-cont
    (val1 expval?)
    (saved-cont continuation?))
  (rator-cont
    (rand expression?)
    (saved-env environment?)
    (saved-cont continuation?))
  (rand-cont
    (val1 expval?)
    (saved-cont continuation?)))

(define (value-of-program pgm)
  (cases program pgm
    (a-program (exp1)
      (set! cont (end-cont))
      (set! exp exp1)
      (set! env (init-env))
      (value-of/k))))

(define (value-of/k)
  (cases expression exp
    (const-exp (num)
      (set! val (num-val num))
      (apply-cont))
    (var-exp (var)
      (set! val (apply-env env var))
      (apply-cont))
    (proc-exp (var body)
      (set! val (proc-val (procedure var body env)))
      (apply-cont))
    (letrec-exp (p-name b-var p-body letrec-body) '())
    (zero?-exp (exp1) '())
    (let-exp (var exp1 body) '())
    (if-exp (exp1 exp2 exp3) '())
    (diff-exp (exp1 exp2) '())
    (call-exp (rator rand) '())
    ))
