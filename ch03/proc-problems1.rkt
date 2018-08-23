#lang eopl

;3.19

;Expression
(define-datatype expression expression?
  (letproc-exp
    (var identifier?)
    (arg identifier?)
    (proc-body expression?)
    (body expression?))
  )

(define (value-of exp env)
  (cases expression exp
    (letproc-exp (var arg proc-body body)
      (let ((proc1 (proc-val (procedure arg proc-body env))))
        (value-of body (extend-env var proc1 ev))))
  ))

;3.20
(proc-exp 'x
 (proc-exp 'y
   (diff-exp (var-exp 'x)
             (diff-exp (const-exp 0)
                       (var-exp 'y)))))

;3.21
(define-datatype expression expression?
  (proc-exp
    (vars (list-of identifier?))
    (body expression?))
  (call-exp
    (rator expression?)
    (rands (list-of expression?)))
  )

(define (value-of exp env)
  (cases expression exp
    (proc-exp (vars body)
      (proc-val (procedure vars body env)))
    (call-exp (rator rands)
      (apply-procedure
       (expval->proc (value-of rator env))
       (map (lambda (rand) (value-of rand env)) rands)))
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