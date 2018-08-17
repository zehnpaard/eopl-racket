#lang eopl

;2.12
(define (empty-stack)
  (list
   (lambda () (eopl:error 'empty-stack "Popping from empty stack"))
   (lambda () (eopl:error 'empty-stack "Peeking the top of empty stack"))
   (lambda () #t)))

(define (push stack val)
  (list
   (lambda () (list val stack))
   (lambda () val)
   (lambda () #f)))

(define (pop stack)
  ((car stack)))

(define (top stack)
  ((cadr stack)))

(define (empty-stack? stack)
  ((caddr stack)))

(define s (empty-stack))
(define t (push (push (push s 1) 2) 3))


;2.13
;2.14
(define (empty-env)
  (list
    (lambda (var)
      (eopl:error 'empty-env "Variable ~s not found" var))
    (lambda ()
      #t)
    (lambda (var)
      #f)))

(define (extend-env var val env)
  (list
    (lambda (search-var)
      (if (eqv? search-var var)
        val
        (apply-env env search-var)))
    (lambda ()
      #f)
    (lambda (search-var)
      (or (eq? search-var var) (has-binding? env search-var)))))

(define (apply-env env var)
  ((car env) var))

(define (empty-env? env)
  ((cadr env)))

(define (has-binding? env var)
  ((caddr env) var))
