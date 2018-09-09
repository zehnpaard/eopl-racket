#lang eopl

;Token scanner specification
(define scanner-spec1
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

; Grammar specification
(define proc-grammar
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
     call-exp)))

; SLLGEN
(sllgen:make-define-datatypes scanner-spec1 proc-grammar)
(define scan-parse (sllgen:make-string-parser scanner-spec1 proc-grammar))

; expval constructors and accessors
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

; environment constructors and accessors

(define identifier? symbol?)

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var identifier?)
   (val expval?)
   (env environment?)))

(define (apply-env e v)
  (cases environment e
    (empty-env ()
      (eopl:error 'apply-env "Variable ~s not found in environment ~s" v e))
    (extend-env (v1 val e1)
      (if (eq? v v1)
        val
        (apply-env e1 v)))))

