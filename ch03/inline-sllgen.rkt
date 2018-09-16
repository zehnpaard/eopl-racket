#lang eopl

; Token scanner
(define scanner-spec
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

; Grammar
(define inline-grammar
  '((program
     (expression)
     a-program)
    (expression
     (number)
     const-exp)
    (expression
     (identifier)
     var-exp)
    (expression
     ("zero? (" expression ")")
     zero?-exp)
    (expression
     ("- (" expression "," expression ")")
     diff-exp)
    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)
    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)
    (expression
     ("proc (" identifier ")" expression)
     proc-exp)
    (expression
     ("(" expression "," expression ")")
     call-exp)
    (expression
     ("%lexref" number)
     nameless-var-exp)
    (expression
     ("%let" expression "in" expression)
     nameless-let-exp)
    (expression
     ("%lexproc" expression)
     nameless-proc-exp)))

; SLLGEN
(sllgen:make-define-datatypes scanner-spec inline-grammar)
(define scan-parse (sllgen:make-string-parser scanner-spec inline-grammar))

; Static environment
(define (empty-senv) '())

(define (extend-senv var senv)
  (cons var senv))

(define (apply-senv senv var)
  (cond
    ((null? senv)
     (eopl:error 'apply-senv "Variable ~s not found" var))
    ((eqv? var (car senv))
     0)
    (else
     (+ 1 (apply-senv (cdr senv) var)))))

; misc
(define identifier? symbol?)

; const-exp environment
(define-datatype const-exps const-exps?
  (empty-const-exps)
  (extend-const-exps
   (var identifier?)
   (val expression?)
   (env const-exps?)))

(define (apply-const-exps ces v)
  (cases const-exps ces
    (empty-const-exps ()
      '())
    (extend-const-exps (var1 val1 env1)
      (if (eq? v var1)
        val1
        (apply-const-exps env1 v)))))

; known-proc environment
(define-datatype known-procs known-procs?
  (empty-known-procs)
  (extend-known-procs
   (var identifier?)
   (body expression?)
   (env known-procs?)))

(define (apply-known-procs kps v)
  (cases known-procs kps
    (empty-known-procs ()
      '())
    (extend-known-procs (var1 exp1 env1)
      (if (eq? v var1)
        exp1
        (apply-known-procs env1 v)))))
