#lang eopl

;2.20
(define (number->bintree n)
  (list '() n '(()) '(())))

(define current-element cadr)

(define (move-to-left b)
  (cons
    (list 'left (car b) (cadr b) (cadddr b))
    (cdr (caddr b))))
(define (move-to-right b)
  (cons
    (list 'right (car b) (cadr b) (caddr b))
    (cdr (cadddr b))))
(define (move-up b)
  (let ((p (car b))
        (b2 (cons '() (cdr b))))
    (if (eqv? 'left (car p))
      (list (cadr p) (caddr p) b2 (cadddr p))
      (list (cadr p) (caddr p) (cadddr p) b2))))

(define (at-leaf? b)
  (= 1 (length b)))
(define (at-root? b)
  (null? (car b)))

(define (insert-to-left n b)
  (list (car b)
        (cadr b)
        (list '() n (caddr b) '(()))
        (cadddr b)))
(define (insert-to-right n b)
  (list (car b)
        (cadr b)
        (caddr b)
        (list '() n (cadddr b) '(()))))
