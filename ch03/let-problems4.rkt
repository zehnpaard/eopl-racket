#lang eopl

;Problems 3.16, 3.17

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
  (var-exp
    (var identifier?))
  (let-exp
    (var-exps (list-of
                (lambda (x)
                  (and (identifier? (car x))
                       (expression? (cadr x))))))
    (body expression?))
  (let*-exp
    (var-exps (list-of
                (lambda (x)
                  (and (identifier? (car x))
                       (expression? (cadr x))))))
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
    (num2->num-exp (op exp1 exp2)
     (num-val
      (op (expval->num (value-of exp1 env))
          (expval->num (value-of exp2 env)))))
    (num->bool-exp (op exp1)
     (bool-val
      (op (expval->num (value-of exp1 env)))))
    (num2->bool-exp (op exp1 exp2)
     (bool-val
      (op (expval->num (value-of exp1 env))
          (expval->num (value-of exp2 env)))))
    (if-exp (exp1 exp2 exp3)
      (if (expval->bool (value-of exp1 env))
        (value-of exp2 env)
        (value-of exp3 env)))
    (var-exp (var)
      (apply-env env var))
    (let-exp (var-exps body)
      (value-of body (extend-env-let var-exps env)))
    (let*-exp (var-exps body)
      (value-of body (extend-env-let* var-exps env)))
    ))

(define (extend-env-let var-exps env)
  (define (extend-env-let-acc var-exps env acc)
    (if (null? var-exps)
      acc
      (extend-env-let-acc
       (cdr var-exps)
       env
       (extend-env (car (car var-exps))
                   (value-of (cadr (car var-exps)) env)
                   acc))))
  (extend-env-let-acc var-exps env env))                          

(define (extend-env-let* var-exps env)
  (if (null? var-exps)
    env
    (extend-env-let*
      (cdr var-exps)
      (extend-env (car (car var-exps))
                  (value-of (cadr (car var-exps)) env)
                  env))))


(define (diff-exp exp1 exp2)
  (num2->num-exp - exp1 exp2))
(define (zero?-exp exp1)
  (num->bool-exp zero? exp1))
(define (equal?-exp exp1 exp2)
  (num2->bool-exp = exp1 exp2))


(value-of-program
 (a-program
  (let-exp (list (list 'x (const-exp 5))
                 (list 'y (diff-exp (var-exp 'x) (const-exp 1))))
           (var-exp 'y))))

(value-of-program
 (a-program
  (let*-exp (list (list 'x (const-exp 5))
                 (list 'y (diff-exp (var-exp 'x) (const-exp 1))))
           (var-exp 'y))))
