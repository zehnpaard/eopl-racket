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

;Problem 3.39

(define nameless-grammar2
  '((expression
     ("emptylist")
     emptylist-exp)
    (expression
     ("cons(" expression "," expression ")")
     cons-expression)))

(define (all p xs)
  (if (null? xs)
    #t
    (and (p (car xs))
         (all p (cdr xs)))))

(define (list-of p)
  (lambda (x)
    (and (list? x)
         (all p x))))

(define-datatype expval expval?
  (list-val
   (vals (list-of expval?))))

(define (expval->list v)
  (cases expval v
    (list-val (l) (map expval->val l))
    (else (eopl:error 'expval->list "Cannot convert ~s to list" v))))

(define (expval->val v)
  (cases expval v
    (num-val (n) n)
    (bool-val (b) b)
    (proc-val (p) p)
    (list-val (l) (map expval->val l))
    (else (eopl:error 'expval->proc "Cannot convert ~s to proc" v))))

(define (value-of e env1)
  (cases expression e
    (emptylist-exp ()
      (list-val '()))
    (cons-exp (exp1 exp2)
      (list-val (cons (value-of exp1 env1)
                      (expval->list (value-of exp2 env1)))))))