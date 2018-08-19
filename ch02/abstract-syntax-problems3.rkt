#lang eopl

(define (identifier? x)
  (and (symbol? x) (not (eqv? x 'lambda))))

(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

(define (all p xs)
  (if (null? xs)
    #t
    (and (p (car xs)) (all p (cdr xs)))))

(define (list-of p)
  (lambda (xs)
    (and (list? xs) (all p xs))))

;2.30
(define (parse-expression datum)
  (cond
    ((identifier? datum)
     (var-exp datum))
    ((not (list? datum))
     (eopl:error 'parse-expression "Could not parse expression ~s. Neither valid symbol nor list" datum))
    ((eqv? (car datum) 'lambda)
     (if (and
          (= 3 (length datum))
          ((list-of identifier?) (cadr datum))
          (= 1 (length (cadr datum))))
       (lambda-exp (cadr datum) (parse-expression (caddr datum)))
       (eopl:error 'parse-expression "Could not parse expression ~s.  Invalid lambda-exp form" datum)))
    ((= 2 (length datum))
     (app-exp (parse-expression (car datum)) (parse-expression (cadr datum))))
    (else
     (eopl:error 'parse-expression "Could not parse expression ~s.  Incorrect number of elements in form" datum))))

;2.31
(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))

(define (parse-pf datum)
  (define (parse-pf-ret datum)
    (cond
      ((integer? (car datum))
       (list (const-exp (car datum)) (cdr datum)))
      ((eqv? (car datum) '-)
       (let ((ret1 (parse-pf-ret (cdr datum))))
         (let ((ret2 (parse-pf-ret (cadr ret1))))
           (list (diff-exp (car ret1) (car ret2)) (cadr ret2)))))
      (else
       (eopl:error 'parse-pf-ret "Unparsable expression ~s" datum))))
  (car (parse-pf-ret datum))) 

