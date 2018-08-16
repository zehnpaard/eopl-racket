#lang eopl

(define (empty-env)
  (list 'empty-env))

(define (extend-env var val env)
  (list 'extend-env var val env))

(define (apply-env env var)
  (cond
    ((eqv? (car env) 'empty-env)
     (report-no-binding-found var))
    ((eqv? (car env) 'extend-env)
     (let ((saved-var (cadr env))
           (saved-val (caddr env))
           (saved-env (cadddr env)))
       (if (eqv? saved-var var)
         saved-val
         (apply-env saved-env var))))
    (else
     (report-invalid-env env))))

(define (report-no-binding-found var)
  (eopl:error 'apply-env "No binding for ~s" var))

(define (report-invalid-env env)
  (eopl:error 'apply-env "Bad environment: ~s" env))
