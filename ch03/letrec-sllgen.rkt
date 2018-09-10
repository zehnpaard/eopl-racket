#lang eopl

; Token scanner specification
(define scanner-spec
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

; Grammar specification
(define letrec-grammar
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
     ("(" expression expression ")")
     call-exp)
    (expression
     ("letrec" identifier "(" identifier ") =" expression "in" expression)
     letrec-exp)))

; SLLGEN
(sllgen:make-define-datatypes scanner-spec letrec-grammar)
(define scan-parse (sllgen:make-string-parser scanner-spec letrec-grammar))
