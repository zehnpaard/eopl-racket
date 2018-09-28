#lang eopl

;4.6
(define (value-of e env1)
  (cases expression e
    (setref-exp (exp1 exp2)
      (let ((val2 (value-of exp2 env1)))
        (begin
          (setref! (expval->ref (value-of exp1 env1))
                   val2)
          val2)))))

;4.7
(define (value-of e env1)
  (cases expression e
    (setref-exp (exp1 exp2)
      (let ((ref1
             (expval->ref (value-of exp1 env1))))
        (let ((val1
               (deref ref1)))
          (begin
            (setref! ref1
                     (value-of exp2 env1))
            val1))))))

;4.8
(define (newref val)
  (set! the-store (append the-store (list val))) ; append is linear
  (- (length the-store) 1))

(define (deref ref)
  (list-ref the-store ref)) ; list-ref is linear

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
        (setref-inner (cdr store1) (- ref1 1)))))) ; recursive setref-inner is worst-case linear
  (set! the-store (setref-inner the-store ref)))

;4.9
(define (empty-store)
  (make-vector 0))

(define the-store 'uninitialized)

(define (get-store)
  the-store)

(define (initialize-store!)
  (set! the-store (empty-store)))

(define (reference? v)
  (integer? v))

(define (newref val)
  (set! the-store (vector-append the-store (make-vector 1 val)))
  (- (vector-length the-store) 1))

(define (deref ref)
  (vector-ref the-store ref))

(define (setref! ref val)
  (vector-set! the-store ref val))

;4.10
(define proc-grammar
  '((program
     (expression)
     a-program)
    (expression-list
     (separated-list expression ";")
     exp-list)
    (expression
     ("begin" expression-list "end")
     begin-exp)))

