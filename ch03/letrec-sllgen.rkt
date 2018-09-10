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

; expval constructors and accessors
(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (proc-val
   (proc proc?)))

(define (expval->num v)
  (cases expval v
    (num-val (n) n)
    (else (eopl:error 'expval->num "Cannot convert ~s to number" v))))
(define (expval->bool v)
  (cases expval v
    (bool-val (b) b)
    (else (eopl:error 'expval->bool "Cannot convert ~s to boolean" v))))
(define (expval->proc v)
  (cases expval v
    (proc-val (p) p)
    (else (eopl:error 'expval->proc "Cannot convert ~s to procedure" v))))

; environment
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
      (eopl:error 'apply-env "Variable ~s not found in environment" v))
    (extend-env (var1 val1 env1)
      (if (eq? var1 v)
        val1
        (apply-env env1 v)))))

; procedure datatype
(define-datatype proc proc?
  (procedure
   (var identifier?)
   (body expression?)
   (penv environment?)))

(define (apply-procedure proc1 val1)
  (cases proc proc1
    (procedure (var body penv)
      (value-of body (extend-env var val penv)))))
