#lang eopl

(define scanner-spec1
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

(define grammar1
  '((statement
     ("{" statement ";" statement "}")
     compount-statement)
    (statement
     ("while" expression "do" statement)
     while-statement)
    (statement
     (identifier ":=" expression)
     assign-statement)
    (expression
     (identifier)
     var-exp)
    (expression
     ("(" expression "-" expression ")")
     diff-exp)))

(sllgen:make-define-datatypes scanner-spec1 grammar1)

(define scanner
  (sllgen:make-string-scanner scanner-spec1 grammar1))

(define scan-parse
  (sllgen:make-string-parser scanner-spec1 grammar1))