#lang eopl

(provide (all-defined-out))

;2.16
(define (var-exp var) var)
(define (lambda-exp var exp) (list 'lambda var exp))
(define (app-exp exp1 exp2) (list exp1 exp2))

(define var-exp? symbol?)
(define (lambda-exp? exp)
  (and
   (list? exp)
   (eqv? 'lambda (car exp))))
(define (app-exp? exp)
  (and
   (list? exp)
   (= 2 ((length exp)))))

(define (var-exp->var exp) exp)
(define lambda-exp->bound-var cadr)
(define lambda-exp->body caddr)
(define app-exp->rator car)
(define app-exp->rand cadr)
