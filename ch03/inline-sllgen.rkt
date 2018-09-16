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

; Translation
(define (translation-of-program p)
  (cases program p
    (a-program (e)
      (a-program
       (translation-of e
                       (empty-senv)
                       (empty-const-exps))))))

(define (translation-of e senv1 ces1)
  (cases expression e
    (const-exp (num)
      (const-exp num))
    (zero?-exp (exp1)
      (zero?-exp (translation-of exp1 senv1 ces1)))
    (diff-exp (exp1 exp2)
      (diff-exp (translation-of exp1 senv1 ces1)
                (translation-of exp2 senv1 ces1)))
    (if-exp (cond-exp true-exp false-exp)
      (if-exp (translation-of cond-exp senv1 ces1)
              (translation-of true-exp senv1 ces1)
              (translation-of false-exp senv1 ces1)))
    (var-exp (var)
      (nameless-var-exp (apply-senv senv1 var)))
    (let-exp (var exp1 body)
      (cases expression exp1
        (const-exp (num)
          (nameless-let-exp
           exp1
           (translation-of body (extend-senv var senv1)
                                (extend-const-exps exp1 ces1))))
        (else
          (nameless-let-exp
           (translation-of exp1 senv1 ces1)
           (translation-of body (extend-senv var senv1) ces1)))))
    (proc-exp (arg body)
      (nameless-proc-exp (translation-of body (extend-senv arg senv1) ces1)))
    (call-exp (func arg)
      (call-exp (translation-of func senv1 ces1)
                (translation-of arg senv1 ces1)))
    (else
     (eopl:error 'translation-of "Unable to translate ~s" e))))
