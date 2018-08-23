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

