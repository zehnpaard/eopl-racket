#lang eopl

(define identifier? symbol?)

(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

;2.28
(define (print-lc-exp exp)
  (cases lc-exp exp
    (var-exp (var)
      (list 'var-exp (list var)))
    (lambda-exp (bound-var body)
      (list 'lambda-exp (list bound-var (print-lc-exp body))))
    (app-exp (rator rand)
      (list 'app-exp (list (print-lc-exp rator) (print-lc-exp rand))))))

(define a (var-exp 'x))
(define b (var-exp 'y))
(define c (lambda-exp 'x a))
(define d (app-exp c b))
