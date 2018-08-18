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

(define (occurs-free? var exp)
  (cases lc-exp exp
    (var-exp (v)
     (eq? v var))
    (lambda-exp (v body)
     (and
      (not (eqv? var v))
      (occurs-free? var body)))
    (app-exp (rator rand)
     (or
      (occurs-free? var rator)
      (occurs-free? var rand)))))

(define-datatype  s-list s-list?
  (empty-s-list)
  (non-empty-s-list
   (first s-exp?)
   (rest s-list?)))
(define-datatype s-exp s-exp?
  (symbol-s-exp
   (sym symbol?))
  (s-list-s-exp
   (slst s-list?)))