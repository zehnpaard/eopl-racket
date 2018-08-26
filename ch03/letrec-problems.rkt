#lang eopl

;3.31
(define-datatype expression expression?
  (proc-exp
    (vars (list-of identifier?))
    (body expression?))
  (call-exp
    (rator expression?)
    (rands (list-of expression?)))
  (letrec-exp
    (proc-name identifier?)
    (args (list-of identifier?))
    (proc-body expression?)
    (let-body expression?))
  )

(define (value-of exp env)
  (cases expression exp
    (proc-exp (vars body)
      (proc-val (procedure vars body env)))
    (call-exp (rator rands)
      (apply-procedure
       (expval->proc (value-of rator env))
       (map (lambda (rand) (value-of rand env)) rands)))
    (letrec-exp (proc-name args proc-body let-body)
       (value-of let-body (extend-env-rec proc-name args proc-body env)))
    ))

(define-datatype proc proc?
  (procedure
   (vars (list-of identifier?))
   (body expression?)
   (env environment?)))

(define (apply-procedure proc1 vals)
  (cases proc proc1
    (procedure (vars body env)
      (value-of body (extend-env* vars vals env)))))
