#lang eopl

;Problems 3.13

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
  (list-val
   (lst (list-of expval?))))

(define (expval->num val)
  (cases expval val
    (num-val (num)
     num)
    (else
     (eopl:error 'expval->num "Cannot convert value ~s to number" val))))


(define (expval->list val)
  (cases expval val
    (list-val (lst)
     lst)
    (else
     (eopl:error 'expval->list "Cannot convert value ~s to list" val))))

(define-datatype program program?
  (a-program
    (exp1 expression?)))

(define-datatype expression expression?
  (const-exp
    (num number?))
  (num2->num-exp
    (op procedure?)
    (exp1 expression?)
    (exp2 expression?))
  (num->bool-exp
    (op procedure?)
    (exp1 expression?))
  (num2->bool-exp
    (op procedure?)
    (exp1 expression?)
    (exp2 expression?))
  (if-exp
    (exp1 expression?)
    (exp2 expression?)
    (exp3 expression?))
  (cond-exp
    (exp-pairs (list-of (list-of expression?))))
  (var-exp
    (var identifier?))
  (let-exp
    (var identifier?)
    (exp1 expression?)
    (body expression?))
  (minus-exp
   (exp1 expression?))
  (cons-exp
   (exp1 expression?)
   (exp2 expression?))
  (emptylist-exp)
  (car-exp
   (exp1 expression?))
  (cdr-exp
   (exp1 expression?))
  (null?-exp
   (exp1 expression?))
  (list-exp
   (exps (list-of expression?))))

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
    (num2->num-exp (op exp1 exp2)
     (num-val
      (op (expval->num (value-of exp1 env))
          (expval->num (value-of exp2 env)))))
    (num->bool-exp (op exp1)
     (num-val
      (if (op (expval->num (value-of exp1 env)))
        1
        0)))
    (num2->bool-exp (op exp1 exp2)
     (num-val
      (if (op (expval->num (value-of exp1 env))
              (expval->num (value-of exp2 env)))
        1
        0)))
    (if-exp (exp1 exp2 exp3)
      (if (zero? (expval->num (value-of exp1 env)))
        (value-of exp3 env)
        (value-of exp2 env)))
    (cond-exp (exp-pairs)
      (value-of-cond exp-pairs env))
    (var-exp (var)
      (apply-env env var))
    (let-exp (var exp1 body)
      (value-of body (extend-env var (value-of exp1 env) env)))
    (minus-exp (exp1)
      (num-val (- (expval->num (value-of exp1 env)))))
    (cons-exp (exp1 exp2)
      (list-val
       (cons (value-of exp1 env)
             (expval->list (value-of exp2 env)))))
    (emptylist-exp ()
      (list-val '()))
    (car-exp (exp1)
      (car (expval->list (value-of exp1 env))))
    (cdr-exp (exp1)
      (list-val
       (cdr (expval->list (value-of exp1 env)))))
    (null?-exp (exp1)
      (num-val
       (if (null? (expval->list exp1))
         1
         0)))
    (list-exp (exps)
      (list-val
       (map (lambda (e) (value-of e env)) exps)))
    ))

(define (value-of-cond pairs env)
  (if (null? pairs)
    (eopl:error 'value-of-cond "No condition succeeded")
    (if (not (zero? (value-of (car (car pairs)) env)))
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

(define exp3
  (cons-exp
   (cons-exp
    (const-exp 1)
    (emptylist-exp))
   (cons-exp
    (const-exp 2)
    (cons-exp
     (const-exp 3)
     (emptylist-exp)))))