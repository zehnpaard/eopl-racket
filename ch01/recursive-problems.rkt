#lang eopl

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

; 1.15
(define (duple n x)
  (if (zero? n)
    '()
    (cons x (duple (dec n) x))))

;1.16
(define (invert xys)
  (define (swap xy)
    (list (cadr xy) (car xy)))
  (if (null? xys)
    xys
    (cons
     (swap (car xys))
     (invert (cdr xys)))))

;1.17
(define (down xs)
  (if (null? xs)
    xs
    (cons
     (list (car xs))
     (down (cdr xs)))))

;1.18
(define (swapper s1 s2 ss)
  (cond
    ((null? ss)
     ss)
    ((eq? (car ss) s1)
     (cons s2 (swapper s1 s2 (cdr ss))))
    ((eq? (car ss) s2)
     (cons s1 (swapper s1 s2 (cdr ss))))
    (else
     (cons (car ss) (swapper s1 s2 (cdr ss))))))

;1.19
(define (list-set xs n x)
  (define (report-list-too-short n)
    (eopl:error 'list-set
      "List too short by ~s elements.~%" (inc n)))
  (cond
    ((null? xs)
     (report-list-too-short n))
    ((zero? n)
     (cons x (cdr xs)))
    (else
     (cons (car xs) (list-set (cdr xs) (dec n) x)))))

;1.20
(define (count-occurrences y xs)
  (if (null? xs)
   0
   (+
     (let ((x (car xs)))
       (if (symbol? x)
         (if (eq? x y) 1 0)
         (count-occurrences y x)))
     (count-occurrences y (cdr xs)))))

;1.21
(define (product ss1 ss2)
  (define (add-tuples s ss ts)
    (if (null? ss)
      ts
      (add-tuples s (cdr ss) (cons (list s (car ss)) ts))))
  (define (product-acc ss1 ss2 ts)
    (if (null? ss1)
      ts
      (product-acc (cdr ss1) ss2 (add-tuples (car ss1) ss2 ts))))
  (product-acc ss1 ss2 '()))

;1.22
(define (filter-in pred xs)
  (if (null? xs)
    xs
    (let ((x (car xs)))
      (if (pred x)
        (cons x (filter-in pred (cdr xs)))
        (filter-in pred (cdr xs))))))

;1.23
(define (list-index pred xs)
  (define (list-index-num pred xs n)
    (cond
      ((null? xs) #f)
      ((pred (car xs)) n)
      (else (list-index-num pred (cdr xs) (inc n)))))
  (list-index-num pred xs 0))

;1.24
(define (every? pred xs)
  (if (null? xs)
    #t
    (and (pred (car xs)) (every? pred (cdr xs)))))

;1.25
(define (exists? pred xs)
  (if (null? xs)
    #f
    (or (pred (car xs)) (exists? pred (cdr xs)))))