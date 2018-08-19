#lang eopl

(define-datatype lc-exp lc-exp?
  (var-exp
   (var symbol?))
  (lambda-exp
   (bound-var symbol?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

(define (parse-expression datum)
  (cond
    ((symbol? datum)
     (var-exp datum))
    ((not (pair? datum))
     (eopl:error 'parse-expression "Invalid expression ~s" datum))
    ((eqv? (car datum) 'lambda)
     (lambda-exp
      (car (cadr datum))
      (parse-expression (caddr datum))))
    (else
     (app-exp
      (parse-expression (car datum))
      (parse-expression (cadr datum))))))

(define (unparse-lc-exp exp)
  (cases lc-exp exp
    (var-exp (var)
      var)
    (lambda-exp (bound-var body)
      (list 'lambda (list bound-var) (unparse-lc-exp body)))
    (app-exp (rator rand)
      (list (unparse-lc-exp rator) (unparse-lc-exp rand)))))
