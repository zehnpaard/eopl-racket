#lang eopl

;2.19
(define (number->bintree n)
  (list n '() '()))

(define current-element car)

(define move-to-left-son cadr)
(define move-to-right-son caddr)

(define (at-leaf? b)
  (and (list? b) (null? b)))

(define (insert-to-left n b)
  (list (car b)
        (list n (cadr b) '())
        (caddr b)))
(define (insert-to-right n b)
  (list (car b)
        (cadr b)
        (list n (caddr b) '())))
