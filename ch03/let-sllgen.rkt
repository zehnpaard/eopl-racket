#lang eopl

; Token scanner specification
(define scanner-spec1
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

; Grammar specification
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

; SLLGEN
(sllgen:make-define-datatypes scanner-spec1 let-grammar)
(define scan-parse (sllgen:make-string-parser scanner-spec1 let-grammar))

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


; interpreter

(define (run s)
  (value-of-program (scan-parse s)))

(define (value-of-program p)
  (cases program p
    (a-program (exp1)
      (value-of exp1 (empty-env)))))

(define (value-of exp1 env1)
  (cases expression exp1
    (const-exp (n)
      (num-val n))
    (var-exp (v)
      (apply-env env1 v))
    (zero?-exp (e)
       (bool-val (zero? (expval->bool (value-of e env1)))))
    (diff-exp (e1 e2)
       (num-val (- (expval->num (value-of e1 env1))
                   (expval->num (value-of e2 env1)))))
    (if-exp (cond-exp true-exp false-exp)
      (if (expval->bool (value-of cond-exp env1))
        (value-of true-exp env1)
        (value-of false-exp env1)))
    (let-exp (v e1 e2)
      (value-of e2 (extend-env v e1 env1)))))