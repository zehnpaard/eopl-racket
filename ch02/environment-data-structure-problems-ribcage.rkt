#lang eopl

;2.11

(define (empty-env)
  '((() ())))

(define (empty-env? env)
  (and
   (= (length env) 1)
   (= (length (car env)) 2)
   (null? (car (car env)))
   (null? (cadr (car env)))))

(define (extend-env var val env)
  (cons (list (cons var (car (car env)))
              (cons val (cadr (car env))))
        (cdr env)))

(define (extend-env* vars vals env)
  (cons (list vars vals) env))


(define (symbol-found? ss s)
  (if (null? ss)
    #f
    (or (eq? (car ss) s) (symbol-found? (cdr ss) s))))

(define (has-binding-sub? subenv var)
  (if (null? subenv)
    #f
    (symbol-found? (car subenv) var)))

(define (apply-subenv subenv var)
    (let ((vars (car subenv))
          (vals (cadr subenv)))
      (cond
        ((null? vars) (eopl:error 'apply-subenv "Var ~s not found in subenv" var))
        ((eq? (car vars) var) (car vals))
        (else (apply-subenv (list (cdr vars) (cdr vals)) var)))))

(define (apply-env env var)
  (cond
    ((null? env) (eopl:error 'apply-env "Var ~s not found in env" var))
    ((has-binding-sub? (car env) var)
     (apply-subenv (car env) var))
    (else
     (apply-env (cdr env) var))))

(define (has-binding? env var)
  (if (null? env)
    #f
    (or (has-binding-sub? (car env) var)
        (has-binding? (cdr env) var))))
