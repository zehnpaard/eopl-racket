#lang eopl

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (list-length xs)
  (if (null? xs)
    0
    (inc (list-length (cdr xs)))))

(define (nth-element xs n)
  (define (report-list-too-short n)
    (eopl:error 'nth-element
      "List too short by ~s elements.~%" (inc n)))
  (if (null? xs)
    (report-list-too-short n)
    (if (zero? n)
      (car xs)
      (nth-element (cdr xs) (dec n)))))

(define (remove-first x xs)
  (cond
    ((null? xs)
     xs)
    ((eq? x (car xs))
     (cdr xs))
    (else
     (cons (car xs) (remove-first x (cdr xs))))))

(define (remove x xs)
  (cond
    ((null? xs)
     xs)
    ((eq? x (car xs))
     (remove x (cdr xs)))
    (else
     (cons (car xs) (remove x (cdr xs))))))

(define (number-elements xs)
  (define (number-elements-from xs n)
    (if (null? xs)
      xs
      (cons
       (list n (car xs))
       (number-elements-from (cdr xs) (inc n)))))
  (number-elements-from xs 0))

(define (list-sum xs)
  (if (null? xs)
    0
    (+ (car xs) (list-sum (cdr xs)))))

(define (list-sum-tco xs)
  (define (list-sum-acc xs acc)
    (if (null? xs)
      acc
      (list-sum-acc (cdr xs) (+ acc (car xs)))))
  (list-sum-acc xs 0))

(define (vector-sum v)
  (define (partial-vector-sum v i)
    (if (negative? i)
      0
      (+ (vector-ref v i)
         (partial-vector-sum v (dec i)))))
  (partial-vector-sum v (dec (vector-length v))))
 

;; lambda calculus

(define (occurs-free? var exp)
  (cond
    ((symbol? exp)
     (eqv? var exp))
    ((eqv? (car exp) 'lambda)
     (and
      (not (eqv? var (car (cadr exp))))
      (occurs-free? var (caddr exp))))
    (else
     (or
      (occurs-free? var (car exp))
      (occurs-free? var (cadr exp))))))

(define (subst new old slist)
  (define (subst-sexp new old sexp)
    (if (symbol? sexp)
      (if (eqv? sexp old) new sexp)
    (subst new old sexp)))
  (if (null? slist)
    '()
    (cons
     (subst-sexp new old (car slist))
     (subst new old (cdr slist)))))