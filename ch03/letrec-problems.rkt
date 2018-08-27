#lang eopl

;3.31
(define-datatype expression expression?
  (proc-exp
    (vars (list-of identifier?))
    (body expression?))
  (call-exp
    (rator expression?)
    (rands (list-of expression?)))
  (letrec-exp
    (proc-name identifier?)
    (args (list-of identifier?))
    (proc-body expression?)
    (let-body expression?))
  )

(define (value-of exp env)
  (cases expression exp
    (proc-exp (vars body)
      (proc-val (procedure vars body env)))
    (call-exp (rator rands)
      (apply-procedure
       (expval->proc (value-of rator env))
       (map (lambda (rand) (value-of rand env)) rands)))
    (letrec-exp (proc-name args proc-body let-body)
       (value-of let-body (extend-env-rec proc-name args proc-body env)))
    ))

(define-datatype proc proc?
  (procedure
   (vars (list-of identifier?))
   (body expression?)
   (env environment?)))

(define (apply-procedure proc1 vals)
  (cases proc proc1
    (procedure (vars body env)
      (value-of body (extend-env* vars vals env)))))

;3.32

(define (extend-env-rec rec-procs env)
  (list rec-procs env))

(define (apply-env env var)
  (cond
   ((null? env)
    (eopl:error 'apply-env "Var ~s not found in env" var))
   ((list? (car (car env)))
    (apply-env-recs env var (car env)))
   ((eqv? var (car (car env)))
    (cadr (car env)))
   (else
    (apply-env (cadr env) var))))

(define (apply-env-recs env var rec-procs)
  (cond
    ((null? rec-procs)
     (apply-env (cadr env) var))
    ((eqv? var (car (car rec-procs)))
     (proc-val (procedure (cadr (car rec-procs)) (caddr (car rec-procs)) env)))
    (else
     (apply-env-recs env var (cdr rec-procs)))))

;Expression
(define-datatype expression expression?
  (letrec-exp
   (rec-procs (list-of rec-proc?))
   (let-body expression?))
  )

(define (rec-proc? x)
  (and
   (list? x)
   (= 3 (length x))
   (identifier? (car x))
   (identifier? (cadr x))
   (expression? (caddr x))))

(define (value-of exp env)
  (cases expression exp
    (letrec-exp (rec-procs let-body)
       (value-of let-body (extend-env-rec rec-procs env)))
  ))


;3.33
(define (extend-env-rec rec-procs env)
  (list rec-procs env))

(define (apply-env env var)
  (cond
   ((null? env)
    (eopl:error 'apply-env "Var ~s not found in env" var))
   ((list? (car (car env)))
    (apply-env-recs env var (car env)))
   ((eqv? var (car (car env)))
    (cadr (car env)))
   (else
    (apply-env (cadr env) var))))

(define (apply-env-recs env var rec-procs)
  (cond
    ((null? rec-procs)
     (apply-env (cadr env) var))
    ((eqv? var (car (car rec-procs)))
     (proc-val (procedure (cadr (car rec-procs)) (caddr (car rec-procs)) env)))
    (else
     (apply-env-recs env var (cdr rec-procs)))))

;Expression
(define-datatype expression expression?
  (proc-exp
    (vars (list-of identifier?))
    (body expression?))
  (call-exp
    (rator expression?)
    (rands (list-of expression?)))
  (letrec-exp
   (rec-procs (list-of rec-proc?))
   (let-body expression?))
  )

(define (rec-proc? x)
  (and
   (list? x)
   (= 3 (length x))
   (identifier? (car x))
   ((list-of identifier?) (cadr x))
   (expression? (caddr x))))

(define (value-of exp env)
  (cases expression exp
    (proc-exp (vars body)
      (proc-val (procedure vars body env)))
    (call-exp (rator rands)
      (apply-procedure
       (expval->proc (value-of rator env))
       (map (lambda (rand) (value-of rand env)) rands)))
    (letrec-exp (rec-procs let-body)
       (value-of let-body (extend-env-rec rec-procs env)))
  ))

(define-datatype proc proc?
  (procedure
   (vars (list-of identifier?))
   (body expression?)
   (env environment?)))

(define (apply-procedure proc1 vals)
  (cases proc proc1
    (procedure (vars body env)
      (value-of body (extend-env* vars vals env)))))

;3.34
(define (empty-env)
  (lambda (var)
    (eopl:error 'empty-env "Variable ~s not found" var)))

(define (extend-env var val env)
  (lambda (search-var)
    (if (eqv? search-var var)
      val
      (apply-env env search-var))))

(define (apply-env env var)
  (env var))

(define (extend-env-rec proc-name arg body)
  (lambda (search-var)
    (if (eqv? search-var proc-name)
      (proc-val (procedure arg body (extend-env-rec proc-name arg body)))
      (apply-env env search-var))))

;3.35
(define (extend-env-rec proc-name arg body env)
  (let ((vec (make-vector 1)))
    (let ((new-env (extend-env proc-name vec env)))
      (vector-set! vec 0
        (proc-val (procedure arg body new-env)))
      new-env)))


(define (apply-env env var)
  (cond
   ((null? env)
    (eopl:error 'apply-env "Var ~s not found in env" var))
   ((eqv? var (car (car env)))
    (if (vector? (cadr (car env)))
      (vector-ref (cadr (car env)) 0)
      (cadr (car env))))
   (else
    (apply-env (cadr env) var))))

;3.36
(define (extend-env-recs rec-procs env)
  (if (null? rec-procs)
    env
    (let ((vec (make-vector 1)))
      (let ((new-env (extend-env-recs
                      (cdr rec-procs)
                      (extend-env (car (car rec-procs))
                                  vec
                                  env))))
        (vector-set! vec 0
          (proc-val
           (procedure (cadr (car rec-procs))
                      (caddr (car rec-procs))
                      new-env)))
        new-env)))
