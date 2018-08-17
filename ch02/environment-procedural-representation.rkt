#lang eopl

(define (empty-env)
  (lambda (var)
    (eopl:error 'empty-env "Variable ~s not found" var)))

(define (extend-env var val env)
  (lambda (search-var)
    (if (eqv? search-var var)
      val
      (apply-env env search-var))))

(define (apply-env env var)
  (env var))