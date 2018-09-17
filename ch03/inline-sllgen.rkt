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
     ("(" expression expression ")")
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
(define (any? pred xs)
  (if (null? xs)
    #f
    (or (pred (car xs))
        (any? pred (cdr xs)))))

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

; known-proc environment
(define-datatype known-procs known-procs?
  (empty-known-procs)
  (extend-known-procs
   (fname identifier?)
   (arg identifier?)
   (body expression?)
   (env known-procs?)))

(define (apply-known-procs kps v)
  (cases known-procs kps
    (empty-known-procs ()
      '())
    (extend-known-procs (fname1 arg1 body1 env1)
      (if (eq? v fname1)
        (list arg1 body1)
        (apply-known-procs env1 v)))))

; Generate known-proc
(define (make-known-proc-body arg body ces)
  (let ((free-vars (get-free-variables body (list arg))))
    (let ((const-exps-list (map (lambda (v) (apply-const-exps ces v))
                           free-vars)))
      (if (any? null? const-exps-list)
        '()
        (let-bind-all free-vars const-exps-list body)))))

(define (let-bind-all free-vars const-exps-list body)
  (if (null? free-vars)
    body
    (let-bind-all (cdr free-vars)
                  (cdr const-exps-list)
                  (let-exp (car free-vars)
                           (car const-exps-list)
                           body))))

(define (get-free-variables e bound-vars)
  (cases expression e
    (const-exp (num)
      '())
    (zero?-exp (exp1)
      (get-free-variables exp1 bound-vars))
    (diff-exp (exp1 exp2)
      (append (get-free-variables exp1 bound-vars)
              (get-free-variables exp2 bound-vars)))
    (if-exp (cond-exp true-exp false-exp)
      (append (get-free-variables cond-exp bound-vars)
              (get-free-variables true-exp bound-vars)
              (get-free-variables false-exp bound-vars)))
    (var-exp (var)
      (if (member var bound-vars)
        '()
        (list var)))
    (let-exp (var exp1 body)
      (append (get-free-variables exp1 bound-vars)
              (get-free-variables body (cons var bound-vars))))
    (proc-exp (arg body)
      (get-free-variables body (cons arg bound-vars)))
    (call-exp (func arg)
      (append (get-free-variables func bound-vars)
              (get-free-variables arg bound-vars)))
    (else
      (eopl:error 'get-free-variables "Cannot get free variables of expression ~s" e))))

; Translation
(define (translation-of-program p)
  (cases program p
    (a-program (e)
      (a-program
       (translation-of e
                       (empty-senv)
                       (empty-const-exps)
                       (empty-known-procs))))))

(define (translation-of e senv1 ces1 kps1)
  (cases expression e
    (const-exp (num)
      (const-exp num))
    (zero?-exp (exp1)
      (zero?-exp (translation-of exp1 senv1 ces1 kps1)))
    (diff-exp (exp1 exp2)
      (diff-exp (translation-of exp1 senv1 ces1 kps1)
                (translation-of exp2 senv1 ces1 kps1)))
    (if-exp (cond-exp true-exp false-exp)
      (if-exp (translation-of cond-exp senv1 ces1 kps1)
              (translation-of true-exp senv1 ces1 kps1)
              (translation-of false-exp senv1 ces1 kps1)))
    (var-exp (var)
      (nameless-var-exp (apply-senv senv1 var)))
    (let-exp (var exp1 body)
      (nameless-let-exp
       (translation-of exp1 senv1 ces1 kps1)
       (cases expression exp1
         (const-exp (num)
           (translation-of body (extend-senv var senv1)
                                (extend-const-exps var exp1 ces1)
                                kps1))
         (proc-exp (arg pbody)
           (translation-of body (extend-senv var senv1)
                                ces1
                                (let ((known-proc-body (make-known-proc-body arg pbody ces1)))
                                  (if (null? known-proc-body)
                                    kps1
                                    (extend-known-procs var arg known-proc-body kps1)))))
        (else
           (translation-of body (extend-senv var senv1) ces1 kps1)))))
    (proc-exp (arg body)
      (nameless-proc-exp (translation-of body (extend-senv arg senv1) ces1 kps1)))
    (call-exp (func arg)
      (cases expression func
        (var-exp (fname)
          (let ((known-proc (apply-known-procs kps1 fname)))
            (if (null? known-proc)
              (call-exp (translation-of func senv1 ces1 kps1)
                        (translation-of arg senv1 ces1 kps1))
              (translation-of
               (let-exp (car known-proc)
                        arg
                        (cadr known-proc))
              senv1 ces1 kps1))))
        (else
          (call-exp (translation-of func senv1 ces1 kps1)
                    (translation-of arg senv1 ces1 kps1)))))
    (else
     (eopl:error 'translation-of "Unable to translate ~s" e))))

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
