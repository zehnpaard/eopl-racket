#lang eopl

;2.4
(define (empty-stack)
  '())

(define push cons)

(define (pop stack)
  (list (car stack) (cdr stack)))

(define top car)

(define empty-stack? null?)

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

;2.8
(define (empty-env? env)
  (null? env))

;2.9
(define (has-binding? env var)
  (if (null? env)
    #f
    (or (eq? (car (car env)) var)
        (has-binding? (cdr env) var))))

;2.10
(define (extend-env* vars vals env)
  (if (or (null? vars) (null? vals))
    env
    (extend-env
     (car vars)
     (car vals)
     (extend-env* (cdr vars) (cdr vals) env))))
