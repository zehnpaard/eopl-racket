#lang eopl

;Problem 3.38

(define nameless-grammar1
  '((expression
     ("cond" "(" (arbno "(" expression expression ")") ")")
     cond-exp)))

(define (translation-of1 e senv1)
  (cases expression e
    (cond-exp (conds vals)
      (cond-exp (map (lambda (x) (translation-of x senv1)) conds)
                (map (lambda (x) (translation-of x senv1)) vals)))))

(define (value-of-cond conds vals env1)
  (cond ((null? conds)
         (eopl:error 'value-of-cond "No conditions matched"))
        ((expval->bool (value-of (car conds) env1))
         (value-of (car vals) env1))
        (else
         (value-of-cond (cdr conds) (cdr vals) env1))))

(define (value-of e env1)
  (cases expression e
    (cond-exp (conds vals)
      (value-of-cond conds vals env1))))