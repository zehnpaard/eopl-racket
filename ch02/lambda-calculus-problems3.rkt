#lang eopl

;2.18
(define (number->sequence n)
  (list n '() '()))

(define current-element car)
(define (move-to-left seq)
  (list (car (cadr seq))
        (cdr (cadr seq))
        (cons (car seq) (caddr seq))))
(define (move-to-right seq)
  (list (car (caddr seq))
        (cons (car seq) (cadr seq))
        (cdr (caddr seq))))
(define (insert-to-left n seq)
  (list (car seq)
        (cons n (cadr seq))
        (caddr seq)))
(define (insert-to-right n seq)
  (list (car seq)
        (cadr seq)
        (cons n (caddr seq))))
