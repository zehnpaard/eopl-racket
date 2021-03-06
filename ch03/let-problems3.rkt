#lang eopl

;Problems 3.14

(define (empty-env)
  '())
(define empty-env? null?)
(define (extend-env var val env)
  (list (list var val) env))
(define (apply-env env var)
  (cond
   ((null? env)
    (eopl:error 'apply-env "Var ~s not found in env" var))
   ((eqv? var (car (car env)))
    (cadr (car env)))
   (else
    (apply-env (cadr env) var))))

(define identifier? symbol?)

(define (all p xs)
  (if (null? xs)
    #t
    (and (p (car xs))
         (all p (cdr xs)))))

(define (list-of p)
  (lambda (xs)
    (and (list? xs)
         (all p xs))))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (list-val
   (lst (list-of expval?))))

(define (expval->num val)
  (cases expval val
    (num-val (num)
     num)
    (else
     (eopl:error 'expval->num "Cannot convert value ~s to number" val))))

(define (expval->bool val)
  (cases expval val
    (bool-val (bool)
     bool)
    (else
     (eopl:error 'expval->bool "Cannot convert value ~s to boolean" val))))

(define (expval->list val)
  (cases expval val
    (list-val (lst)
     lst)
    (else
     (eopl:error 'expval->list "Cannot convert value ~s to list" val))))

(define-datatype program program?
  (a-program
    (exp1 expression?)))

(define-datatype bool-exp bool-exp?
  (num->bool-exp
    (op procedure?)
    (exp1 expression?))
  (num2->bool-exp
    (op procedure?)
    (exp1 expression?)
    (exp2 expression?)))

(define-datatype expression expression?
  (const-exp
    (num number?))
  (boolean-exp
    (boolexp bool-exp?))
  (num2->num-exp
    (op procedure?)
    (exp1 expression?)
    (exp2 expression?))
  (if-exp
    (bexp bool-exp?)
    (exp2 expression?)
    (exp3 expression?))
  (cond-exp
    (exp-pairs (list-of
                (lambda (x)
                  (and (bool-exp? (car x))
                       (expression? (cadr x)))))))
  (var-exp
    (var identifier?))
  (let-exp
    (var identifier?)
    (exp1 expression?)
    (body expression?))
  )

(define (init-env)
  (extend-env
    'i (num-val 1)
    (extend-env
      'v (num-val 5)
      (extend-env
        'x (num-val 10)
        (empty-env)))))

(define (value-of-program pgm)
  (cases program pgm
    (a-program (exp1)
      (value-of exp1 (init-env)))))

(define (value-of exp env)
  (cases expression exp
    (const-exp (num)
     (num-val num))
    (boolean-exp (boolexp)
     (value-of-bool-exp boolexp env))
    (num2->num-exp (op exp1 exp2)
     (num-val
      (op (expval->num (value-of exp1 env))
          (expval->num (value-of exp2 env)))))
    (if-exp (boolexp exp1 exp2)
      (if (expval->bool (value-of-bool-exp boolexp env))
        (value-of exp1 env)
        (value-of exp2 env)))
    (cond-exp (exp-pairs)
      (value-of-cond exp-pairs env))
    (var-exp (var)
      (apply-env env var))
    (let-exp (var exp1 body)
      (value-of body (extend-env var (value-of exp1 env) env)))
    ))

(define (value-of-bool-exp exp env)
  (cases bool-exp exp
    (num->bool-exp (op exp1)
     (bool-val
      (op (expval->num (value-of exp1 env)))))
    (num2->bool-exp (op exp1 exp2)
     (bool-val
      (op (expval->num (value-of exp1 env))
          (expval->num (value-of exp2 env)))))))

(define (value-of-cond pairs env)
  (if (null? pairs)
    (eopl:error 'value-of-cond "No condition succeeded")
    (if (expval->bool (value-of-bool-exp (car (car pairs)) env))
      (value-of (cadr (car pairs)) env)
      (value-of-cond (cdr pairs) env))))

(define (diff-exp exp1 exp2)
  (num2->num-exp - exp1 exp2))
(define (add-exp exp1 exp2)
  (num2->num-exp + exp1 exp2))
(define (mul-exp exp1 exp2)
  (num2->num-exp * exp1 exp2))
(define (quot-exp exp1 exp2)
  (num2->num-exp quotient exp1 exp2))

(define (zero?-exp exp1)
  (num->bool-exp zero? exp1))

(define (equal?-exp exp1 exp2)
  (num2->bool-exp = exp1 exp2))
(define (greater?-exp exp1 exp2)
  (num2->bool-exp > exp1 exp2))
(define (less?-exp exp1 exp2)
  (num2->bool-exp < exp1 exp2))
