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
   (bool boolean?))
  (proc-val
   (proc proc?)))

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

(define expval->proc
  (lambda (v)
    (cases expval v
      (proc-val (p) p)
      (else (eopl:error 'expval->proc "Cannot convert non-proc-val ~s to proc" v)))))

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

; procedure datatype
(define-datatype proc proc?
  (procedure
   (var identifier?)
   (body expression?)
   (penv environment?)))

(define (apply-procedure proc1 val)
  (cases proc proc1
    (procedure (var body penv)
       (value-of body (extend-env var val penv)))))

; store
(define (empty-store)
  '())

(define the-store 'uninitialized)

(define (get-store)
  the-store)

(define (initialize-store)
  (set! the-store (empty-store)))

(define (reference? v)
  (integer? v))

(define (newref val)
  (let ((next-ref (length the-store)))
    (set! the-store (append the-store (list val)))
    (next-ref)))

(define (deref ref)
  (list-ref the-store ref))

(define (setref! ref val)
  (define (setref-inner store1 ref1)
    (cond
      ((null? store1)
       (eopl:error 'setref-inner "Invalid reference ~s" ref1))
      ((zero? ref1)
       (cons val (cdr store1)))
      (else
       (cons
        (car store1)
        (setref-inner (cdr store1) (- ref1 1))))))
  (set! the-store (setref-inner the-store ref)))

; Interpreter

(define (run s)
  (value-of-program (scan-parse s)))

(define (value-of-program p)
  (cases program p
    (a-program (e)
      (value-of e (empty-env)))))

(define (value-of e env1)
  (cases expression e
    (const-exp (n)
      (num-val n))
    (var-exp (v)
      (apply-env env1 v))
    (zero?-exp (e)
      (bool-val (zero? (expval->num (value-of e env1)))))
    (diff-exp (e1 e2)
      (num-val (- (expval->num (value-of e1 env1))
                  (expval->num (value-of e2 env1)))))
    (if-exp (cond-e true-e false-e)
      (if (expval->bool (value-of cond-e env1))
        (value-of true-e env1)
        (value-of false-e env1)))
    (let-exp (v e1 e2)
      (value-of e2 (extend-env v (value-of e1 env1) env1)))
    (proc-exp (v e1)
      (proc-val (procedure v e1 env1)))
    (call-exp (e1 e2)
      (apply-procedure
       (expval->proc (value-of e1 env1))
       (value-of e2 env1)))))