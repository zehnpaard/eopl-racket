#lang eopl
(require "lambda-calculus-problems1.rkt")

(define (occurs-free? var exp)
  (cond
    ((var-exp? exp)
     (eqv? var (var-exp->var exp)))
    ((lambda-exp? exp)
     (and
      (not (eqv? var (lambda-exp->bound-var exp)))
      (occurs-free? var (lambda-exp->body exp))))
    (else
     (or
      (occurs-free? var (app-exp->rator exp))
      (occurs-free? var (app-exp->rand exp))))))
