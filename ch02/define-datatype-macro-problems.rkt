#lang eopl

;2.21
(define (true x) #t)
(define-datatype env env?
  (empty-env)
  (non-empty-env
   (saved-var symbol?)
   (saved-val true)
   (saved-env env?)))

(define extend-env non-empty-env)

(define (apply-env env1 var)
  (cases env env1
    (empty-env ()
     (eopl:error 'apply-env "Variable ~s not found" var))
    (non-empty-env (var2 val2 env2)
     (if (eqv? var var2)
       val2
       (apply-env env2 var)))))

(define (has-binding? env1 var)
  (cases env env1
    (empty-env ()
      #f)
    (non-empty-env (var2 val2 env2)
     (or
      (eqv? var var2)
      (has-binding? env2 var)))))

;2.22
(define-datatype stack stack?
  (empty-stack)
  (non-empty-stack
   (val true)
   (rest-stack stack?)))

(define push non-empty-stack)
(define (pop s)
  (cases stack s
    (empty-stack ()
     (eopl:error 'pop "Popping from empty stack"))
    (non-empty-stack (v r)
     (list v r))))
(define (top s)
  (cases stack s
    (empty-stack ()
     (eopl:error 'top "Peeking top of empty stack"))
    (non-empty-stack (v r)
     v)))
(define (empty-stack? s)
  (cases stack s
    (empty-stack () #t)
    (non-empty-stack (v r) #f)))