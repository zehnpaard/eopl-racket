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
