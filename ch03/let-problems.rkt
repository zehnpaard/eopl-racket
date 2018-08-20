#lang eopl

;Problems 3.6, 3.7, 3.8

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

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?)))

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

(define-datatype program program?
  (a-program
    (exp1 expression?)))

(define-datatype expression expression?
  (const-exp
    (num number?))
  (diff-exp
    (exp1 expression?)
    (exp2 expression?))
  (add-exp
    (exp1 expression?)
    (exp2 expression?))
  (mul-exp
    (exp1 expression?)
    (exp2 expression?))
  (quot-exp
    (exp1 expression?)
    (exp2 expression?))
  (zero?-exp
    (exp1 expression?))
  (equal?-exp
    (exp1 expression?)
    (exp2 expression?))
  (greater?-exp
    (exp1 expression?)
    (exp2 expression?))
  (less?-exp
    (exp1 expression?)
    (exp2 expression?))
  (if-exp
    (exp1 expression?)
    (exp2 expression?)
    (exp3 expression?))
  (var-exp
    (var identifier?))
  (let-exp
    (var identifier?)
    (exp1 expression?)
    (body expression?))
  (minus-exp
   (exp1 expression?)))

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
    (diff-exp (exp1 exp2)
     (num-val
      (- (expval->num (value-of exp1 env))
         (expval->num (value-of exp2 env)))))
    (add-exp (exp1 exp2)
     (num-val
      (+ (expval->num (value-of exp1 env))
         (expval->num (value-of exp2 env)))))
    (mul-exp (exp1 exp2)
     (num-val
      (* (expval->num (value-of exp1 env))
         (expval->num (value-of exp2 env)))))
    (quot-exp (exp1 exp2)
     (num-val
      (quotient (expval->num (value-of exp1 env))
                (expval->num (value-of exp2 env)))))
    (zero?-exp (exp1)
     (bool-val
      (zero? (expval->num (value-of exp1 env)))))
    (equal?-exp (exp1 exp2)
     (bool-val
      (= (expval->num (value-of exp1 env))
         (expval->num (value-of exp2 env)))))
    (greater?-exp (exp1 exp2)
     (bool-val
      (> (expval->num (value-of exp1 env))
         (expval->num (value-of exp2 env)))))
    (less?-exp (exp1 exp2)
     (bool-val
      (< (expval->num (value-of exp1 env))
         (expval->num (value-of exp2 env)))))
    (if-exp (exp1 exp2 exp3)
      (if (expval->bool (value-of exp1 env))
        (value-of exp2 env)
        (value-of exp3 env)))
    (var-exp (var)
      (apply-env env var))
    (let-exp (var exp1 body)
      (value-of body (extend-env var (value-of exp1 env) env)))
    (minus-exp (exp1)
      (num-val (- (expval->num (value-of exp1 env)))))))

(define exp1
  (diff-exp (var-exp 'x) (const-exp 2)))

(define pgm1
  (a-program exp1))

(define exp2
  (if-exp (zero?-exp (diff-exp (const-exp 1) (var-exp 'i)))
    (let-exp 'y (diff-exp (const-exp 5) (const-exp 2))
      (diff-exp (var-exp 'y) (const-exp 10)))
    exp1))

(define pgm2
  (a-program exp2))

