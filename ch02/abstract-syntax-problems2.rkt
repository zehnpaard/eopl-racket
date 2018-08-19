#lang eopl

(define identifier? symbol?)

(define (all p xs)
  (if (null? xs)
    #t
    (and
     (p (car xs))
     (all p (cdr xs)))))

(define (list-of p)
  (lambda (xs)
    (and
     (list? xs)
     (all p xs))))

;2.29
(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-vars (list-of identifier?))
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rands (list-of lc-exp?))))
   
(define (parse-expression datum)
  (cond
    ((symbol? datum)
     (var-exp datum))
    ((not (pair? datum))
     (eopl:error 'parse-expression "Could not parse expression ~s" datum))
    ((eqv? (car datum) 'lambda)
     (lambda-exp
      (cadr datum)
      (parse-expression (caddr datum))))
    (else
     (app-exp
      (parse-expression (car datum))
      (map parse-expression (cadr datum))))))

(define datum '((lambda (x y z) (x (y z))) (a b c)))
