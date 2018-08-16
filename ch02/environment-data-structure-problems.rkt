#lang eopl

;2.5
(define (empty-env)
  '())

(define (extend-env var val env)
  (cons (list var val) env))

(define (apply-env env var)
  (cond
    ((null? env)
     (report-error))
    ((eqv? (car (car env)) var)
     (cadr (car env)))
    (else
     (apply-env (cdr env) var))))

(define (report-error)
  (eopl:error 'apply-env "Some error!"))
