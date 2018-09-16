#lang eopl

; Token scanner
(define scanner-spec
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

; Grammar
(define nameless-grammar
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
