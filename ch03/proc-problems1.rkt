#lang eopl

;3.19

;Expression
(define-datatype expression expression?
  (letproc-exp
    (var identifier?)
    (arg identifier?)
    (proc-body expression?)
    (body expression?))
  )

(define (value-of exp env)
  (cases expression exp
    (letproc-exp (var arg proc-body body)
      (let ((proc1 (proc-val (procedure arg proc-body env))))
        (value-of body (extend-env var proc1 ev))))
  ))

;3.20
(proc-exp 'x
 (proc-exp 'y
   (diff-exp (var-exp 'x)
             (diff-exp (const-exp 0)
                       (var-exp 'y)))))

;3.21
(define-datatype expression expression?
  (proc-exp
    (vars (list-of identifier?))
    (body expression?))
  (call-exp
    (rator expression?)
    (rands (list-of expression?)))
  )

(define (value-of exp env)
  (cases expression exp
    (proc-exp (vars body)
      (proc-val (procedure vars body env)))
    (call-exp (rator rands)
      (apply-procedure
       (expval->proc (value-of rator env))
       (map (lambda (rand) (value-of rand env)) rands)))
    ))

(define-datatype proc proc?
  (procedure
   (vars (list-of identifier?))
   (body expression?)
   (env environment?)))

(define (apply-procedure proc1 vals)
  (cases proc proc1
    (procedure (vars body env)
      (value-of body (extend-env* vars vals env)))))

;3.23
(define quadruple1
  (let-exp 'makemult
    (proc-exp 'maker
      (proc-exp 'x
        (if-exp (zero?-exp (var-exp 'x))
          (const-exp 0)
          (diff-exp
           (call-exp
            (call-exp (var-exp 'maker) (var-exp 'maker))
            (diff-exp (var-exp 'x) (const-exp 1)))
           (const-exp -4)
                ))))
    (let-exp 'times4
      (proc-exp 'x
        (call-exp
          (call-exp (var-exp 'makemult) (var-exp 'makemult))
          (var-exp 'x)))
      (call-exp (var-exp 'times4) (const-exp 3)))))

(define times1
  (proc-exp 'x
    (proc-exp 'y
      (let-exp 'makemult
        (proc-exp 'maker
          (proc-exp 'z
            (if-exp (zero?-exp (var-exp 'z))
              (const-exp 0)
              (diff-exp
               (call-exp
                (call-exp (var-exp 'maker) (var-exp 'maker))
                (diff-exp (var-exp 'z) (const-exp 1)))
               (diff-exp (const-exp 0) (var-exp 'y))))))
        (call-exp
          (call-exp (var-exp 'makemult) (var-exp 'makemult))
          (var-exp 'x))))))

(define factorial1
  (proc-exp 'x
    (let-exp 'makemult
      (proc-exp 'maker
        (proc-exp 'y
          (if-exp (zero?-exp (var-exp 'y))
            (const-exp 1)
            (call-exp
             (call-exp times-proc (var-exp 'y))
             (call-exp
              (call-exp (var-exp 'maker) (var-exp 'maker))
              (diff-exp (var-exp 'y) (const-exp 1)))))))
      (call-exp
        (call-exp (var-exp 'makemult) (var-exp 'makemult))
        (var-exp 'x)))))

(define factorial2
  (proc-exp 'x
    (let-exp 'times
      (proc-exp 'x
        (proc-exp 'y
          (let-exp 'makemult
            (proc-exp 'maker
              (proc-exp 'z
                (if-exp (zero?-exp (var-exp 'z))
                  (const-exp 0)
                  (diff-exp
                   (call-exp
                    (call-exp (var-exp 'maker) (var-exp 'maker))
                    (diff-exp (var-exp 'z) (const-exp 1)))
                   (diff-exp (const-exp 0) (var-exp 'y))))))
            (call-exp
              (call-exp (var-exp 'makemult) (var-exp 'makemult))
              (var-exp 'x)))))
      (let-exp 'makemult
        (proc-exp 'maker
          (proc-exp 'y
            (if-exp (zero?-exp (var-exp 'y))
              (const-exp 1)
              (call-exp
               (call-exp (var-exp 'times) (var-exp 'y))
               (call-exp
                (call-exp (var-exp 'maker) (var-exp 'maker))
                (diff-exp (var-exp 'y) (const-exp 1)))))))
        (call-exp
          (call-exp (var-exp 'makemult) (var-exp 'makemult))
          (var-exp 'x))))))

;3.24
(define odd-proc1
  (let-exp 'odd
    (proc-exp 'next-p
      (proc-exp 'nnext-p
        (proc-exp 'x
          (if-exp (zero?-exp (var-exp 'x))
            (zero?-exp (const-exp 1))
            (call-exp
              (call-exp
                (call-exp
                  (var-exp 'next-p)
                  (var-exp 'nnext-p))
                (var-exp 'next-p))
              (diff-exp (var-exp 'x) (const-exp 1)))))))
    (let-exp 'even
      (proc-exp 'next-p
        (proc-exp 'nnext-p
          (proc-exp 'x
            (if-exp (zero?-exp (var-exp 'x))
              (zero?-exp (const-exp 0))
              (call-exp
                (call-exp
                  (call-exp
                    (var-exp 'next-p)
                    (var-exp 'nnext-p))
                  (var-exp 'next-p))
                (diff-exp (var-exp 'x) (const-exp 1)))))))
      (call-exp
        (call-exp (var-exp 'odd) (var-exp 'even))
        (var-exp 'odd)))))

(define odd-proc2
  (let-exp 'm
    (proc-exp 'maker
      (proc-exp 'ret
        (proc-exp 'next-ret
          (proc-exp 'x
            (if-exp (zero?-exp (var-exp 'x))
              (var-exp 'ret)
              (call-exp
                (call-exp
                  (call-exp
                    (call-exp
                      (var-exp 'maker)
                      (var-exp 'maker))
                    (var-exp 'next-ret))
                  (var-exp 'ret))
                (diff-exp (var-exp 'x) (const-exp 1))))))))
    (call-exp
     (call-exp
      (call-exp (var-exp 'm) (var-exp 'm))
      (zero?-exp (const-exp 1)))
     (zero?-exp (const-exp 0)))))

;3.25
(define y
  (proc-exp 'f
    (let-exp 'd
      (proc-exp 'x
        (proc-exp 'z
          (call-exp
            (call-exp
              (var-exp 'f)
              (call-exp
                (var-exp 'x)
                (var-exp 'x)))
            (var-exp 'z))))
      (proc-exp 'n
        (call-exp
          (call-exp
            (var-exp 'f)
            (call-exp
              (var-exp 'd)
              (var-exp 'd)))
          (var-exp 'n))))))

(define maketimes
  (proc-exp 'f
    (proc-exp 'x
      (if-exp (zero?-exp (var-exp 'x))
        (const-exp 0)
        (diff-exp
         (call-exp
          (var-exp 'f)
          (diff-exp (var-exp 'x)
                    (const-exp 1)))
         (const-exp -4))))))

(value-of-program (a-program (call-exp (call-exp y maketimes) (const-exp 3))))

;3.26

(define (remove-var var vars)
  (cond
    ((null? vars)
     vars)
    ((eqv? var (car vars))
     (remove-var var (cdr vars)))
    (else
     (cons (car vars)
           (remove-var var (cdr vars))))))

(define (free-vars exp1)
  (cases expression exp1
    (var-exp (var)
      (list var))
    (const-exp (num)
      '())
    (diff-exp (exp1 exp2)
      (append (free-vars exp1)
              (free-vars exp2)))
    (zero?-exp (exp1)
      (free-vars exp1))
    (if-exp (exp1 exp2 exp3)
      (append (free-vars exp1)
              (free-vars exp2)
              (free-vars exp3)))
    (call-exp (rator rand)
      (append (free-vars rator)
              (free-vars rand)))
    (let-exp (var exp1 body)
      (append (free-vars exp1)
              (remove-var
                var
                (free-vars body))))
    (proc-exp (var body)
      (remove-var
        var
        (free-vars body)))
  ))

(define (create-proc-env var body env)
  (let ((vars (remove-var var (free-vars body))))
    (extend-env* vars (map (lambda (v) (apply-env env v)) vars) (empty-env))))

(define (value-of exp env)
  (cases expression exp
    (proc-exp (var body)
      (proc-val (procedure var body (create-proc-env var body env))))
    ))

;3.27

(define-datatype expression expression?
  (traceproc-exp
    (var identifier?)
    (body expression?))
  )

(define (value-of exp env)
  (cases expression exp
    (traceproc-exp (var body)
      (proc-val (trace-procedure var body (create-proc-env var body env))))
    ))

(define-datatype proc proc?
  (procedure
   (var identifier?)
   (body expression?)
   (env environment?))
  (trace-procedure
   (var identifier?)
   (body expression?)
   (env environment?)))

(define (apply-procedure proc1 val)
  (cases proc proc1
    (procedure (var body env)
      (value-of body (extend-env var val env)))
    (trace-procedure (var body env)
      (begin
        (write "enter")
        (newline)
        (let ((res (value-of body (extend-env var val env))))
          (begin
            (write "exit")
            (newline)
            res))))
  ))
