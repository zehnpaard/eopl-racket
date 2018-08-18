#lang eopl

;2.21
(define (true x) #t)
(define-datatype env env?
  (empty-env)
  (non-empty-env
   (saved-var symbol?)
   (saved-val true)
   (saved-env env?)))

(define extend-env non-empty-env)

(define (apply-env env1 var)
  (cases env env1
    (empty-env ()
     (eopl:error 'apply-env "Variable ~s not found" var))
    (non-empty-env (var2 val2 env2)
     (if (eqv? var var2)
       val2
       (apply-env env2 var)))))

(define (has-binding? env1 var)
  (cases env env1
    (empty-env ()
      #f)
    (non-empty-env (var2 val2 env2)
     (or
      (eqv? var var2)
      (has-binding? env2 var)))))

;2.22
(define-datatype stack stack?
  (empty-stack)
  (non-empty-stack
   (val true)
   (rest-stack stack?)))

(define push non-empty-stack)
(define (pop s)
  (cases stack s
    (empty-stack ()
     (eopl:error 'pop "Popping from empty stack"))
    (non-empty-stack (v r)
     (list v r))))
(define (top s)
  (cases stack s
    (empty-stack ()
     (eopl:error 'top "Peeking top of empty stack"))
    (non-empty-stack (v r)
     v)))
(define (empty-stack? s)
  (cases stack s
    (empty-stack () #t)
    (non-empty-stack (v r) #f)))

;2.23
(define (identifier? x)
  (and (symbol? x) (not (eqv? x 'lambda))))
(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

;2.24
(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))
(define (bintree-to-list b)
  (cases bintree b
    (leaf-node (num)
      (list 'leaf-node num))
    (interior-node (key left right)
      (list 'interior-node
            key
            (bintree-to-list left)
            (bintree-to-list right)))))

;2.25

(define (max-interior b)
  (define (node-sum-list-acc node acc)
    (cases bintree node
      (leaf-node (n)
        (cons (list '() n) acc))
      (interior-node (key left right)
        (let ((x (node-sum-list-acc left acc)))
          (let ((y (node-sum-list-acc right x)))
            (cons (list key (+ (cadr (car x)) (cadr (car y))))
                  y))))))
  (define (filter-leafs xs)
    (cond
      ((null? xs) xs)
      ((eqv? '() (car (car xs)))
       (filter-leafs (cdr xs)))
      (else
       (cons (car xs) (filter-leafs (cdr xs))))))
  (define (node-sum-list node)
    (filter-leafs (node-sum-list-acc node '())))
  (define (max-node-acc xs y)
    (cond
      ((null? xs)
       y)
      ((> (cadr (car xs)) (cadr y))
       (max-node-acc (cdr xs) (car xs)))
      (else
       (max-node-acc (cdr xs) y))))
  (define (max-node xs)
    (max-node-acc xs (car xs)))
  (car (max-node (node-sum-list b))))

;2.26
(define (all p xs)
  (if (null? xs)
    #t
    (and (p (car xs))
         (all p (cdr xs)))))

(define (list-of p)
  (lambda (xs)
    (and
     (list? xs)
     (all p xs))))

(define-datatype red-blue-st red-blue-st?
  (red-node
   (left red-blue-st?)
   (right red-blue-st?))
  (blue-node
   (trees (list-of red-blue-st?)))
  (rbleaf-node
   (num number?)))

(define (inc x) (+ x 1))

(define (mark-leaves-with-red-depth rbt)
  (define (mlwrd rbt acc)
    (cases red-blue-st rbt
      (red-node (left right)
        (red-node (mlwrd left (inc acc)) (mlwrd right (inc acc))))
      (blue-node (trees)
        (map-mlwrd trees acc))
      (rbleaf-node (num)
        (rbleaf-node acc))))
  (define (map-mlwrd trees acc)
    (if (null? trees)
      trees
      (cons
       (mlwrd (car trees) acc)
       (map-mlwrd (cdr trees) acc))))
  (mlwrd rbt 0))