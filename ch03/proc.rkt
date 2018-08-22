#lang eopl

;Define environment
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
(define (environment? x) #t) ;TODO: fix


;Expvals
(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (proc-val
   (proc proc?)))

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

(define (expval->proc val)
  (cases expval val
    (proc-val (proc)
      proc)
    (else
     (eopl:error 'expval->proc "Cannot convert value ~s to procedure" val))))


;Miscellaneous defines
(define identifier? symbol?)

(define (init-env)
  (extend-env
    'i (num-val 1)
    (extend-env
      'v (num-val 5)
      (extend-env
        'x (num-val 10)
        (empty-env)))))

  
;Program
(define-datatype program program?
  (a-program
    (exp1 expression?)))

(define (value-of-program pgm)
  (cases program pgm
    (a-program (exp1)
      (value-of exp1 (init-env)))))


;Expression
(define-datatype expression expression?
  (const-exp
    (num number?))
  (diff-exp
    (exp1 expression?)
    (exp2 expression?))
  (zero?-exp
    (exp1 expression?))
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
  (proc-exp
    (var identifier?)
    (body expression?))
  (call-exp
    (rator expression?)
    (rand expression?))
  )

(define (value-of exp env)
  (cases expression exp
    (const-exp (num)
     (num-val num))
    (diff-exp (exp1 exp2)
     (num-val
      (- (expval->num (value-of exp1 env))
         (expval->num (value-of exp2 env)))))
    (zero?-exp (exp1)
     (bool-val
      (zero? (expval->num (value-of exp1 env)))))
    (if-exp (exp1 exp2 exp3)
      (if (expval->bool (value-of exp1 env))
        (value-of exp2 env)
        (value-of exp3 env)))
    (var-exp (var)
      (apply-env env var))
    (let-exp (var exp1 body)
      (value-of body (extend-env var (value-of exp1 env) env)))
    (proc-exp (var body)
      (proc-val (procedure var body env)))
    (call-exp (rator rand)
      (apply-procedure
       (expval->proc (value-of rator env))
       (value-of rand env)))
    ))


;Procedure
(define-datatype proc proc?
  (procedure
   (var identifier?)
   (body expression?)
   (env environment?)))

(define (apply-procedure proc1 val)
  (cases proc proc1
    (procedure (var body env)
      (value-of body (extend-env var val env)))))