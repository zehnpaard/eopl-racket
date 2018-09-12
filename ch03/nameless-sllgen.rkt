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

; SLLGEN
(sllgen:make-define-datatypes scanner-spec nameless-grammar)
(define scan-parse (sllgen:make-string-parser scanner-spec nameless-grammar))

; Translation
(define (translation-of-program p)
  (cases program p
    (a-program (e)
      (a-program
       (translation-of e (empty-senv))))))

(define (translation-of e senv1)
  (cases expression e
    (const-exp (num)
      (const-exp num))
    (zero?-exp (exp1)
      (zero?-exp (translation-of exp1 senv1)))
    (diff-exp (exp1 exp2)
      (diff-exp (translation-of exp1 senv1)
                (translation-of exp2 senv1)))
    (if-exp (cond-exp true-exp false-exp)
      (if-exp (translation-of if-exp senv1)
              (translation-of true-exp senv1)
              (translation-of false-exp senv1)))
    (var-exp (var)
      (nameless-var-exp (apply-senv senv1 var)))
    (let-exp (var exp1 body)
      (nameless-let-exp
       (translation-of exp1 senv1)
       (translation-of body (extend-senv var senv1))))
    (proc-exp (arg body)
      (nameless-proc-exp
       (translation-of body (extend-senv arg senv1))))
    (call-exp (func arg)
       (call-exp (translation-of func senv1)
                 (translation-of arg senv1)))
    (else
     (eopl:error 'translation-of "Unable to translate expression ~s" e))))

; expressions
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
    (else (eopl:error 'expval->proc "Cannot convert ~s to proc" v))))

; nameless environment
(define (nameless-environment? x)
  ((list-of expval?) x))

(define (empty-nameless-env)
  '())

(define (extend-nameless-env val nenv)
  (cons val nenv))

(define (apply-nameless-env nenv n)
  (list-ref nenv n))

; procedure
(define-datatype proc proc?
  (procedure
   (body expression?)
   (penv nameless-environment?)))

(define (apply-procedure proc1 arg)
  (cases proc proc1
    (procedure (body penv)
      (value-of body (extend-nameless-env arg penv)))))

; interpreter

(define (value-of e env1)
  (cases expression e
    (const-exp (n)
      (num-val n))
    (zero?-exp (exp1)
      (bool-val (zero? (expval->num (value-of exp1 env1)))))
    (diff-exp (exp1 exp2)
      (num-val (- (expval->num (value-of exp1 env1))
                  (expval->num (value-of exp2 env1)))))
    (if-exp (cond-exp true-exp false-exp)
      (if (expval->bool (value-of cond-exp env1))
        (value-of true-exp env1)
        (value-of false-exp env1)))
    (call-exp (func arg)
      (apply-procedure
       (expval->proc (value-of func env1))
       (value-of arg env1)))
    (nameless-var-exp (n)
      (apply-nameless-env env1 n))
    (nameless-let-exp (exp1 body)
      (value-of body
        (extend-nameless-env (value-of exp1 env1) env1)))
    (nameless-proc-exp (body)
      (proc-val (procedure body env1)))
    (else
      (eopl:error 'value-of "Cannot get value of expression ~s" e))))

(define (value-of-program p)
  (cases program p
    (a-program (e)
      (value-of e (empty-nameless-env)))))

(define (run s)
  (value-of-program
   (translation-of-program (scan-parse s))))