#lang eopl

(define scanner-spec1
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

(define let-grammar
  '((program
     (expression)
     a-program)
    (expression
     (number)
     const-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)
    (expression
     ("zero? (" expression ")")
     zero?-exp)
    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)
    (expression
     (identifier)
     var-exp)
    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)))

(sllgen:make-define-datatypes scanner-spec1 let-grammar)
(define scan-parse (sllgen:make-string-parser scanner-spec1 let-grammar))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?)))

(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (n) n)
      (else (eopl:error 'expval->num "Cannot convert non-num-val ~s to number" v)))))

(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (b) b)
      (else (eopl:error 'expval->bool "Cannot convert non-bool-val ~s to boolean" v)))))