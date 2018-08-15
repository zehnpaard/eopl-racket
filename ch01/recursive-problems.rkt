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

;1.26
(define (up xs)
  (define (concat xs ys)
    (if (null? xs)
      ys
      (cons (car xs) (concat (cdr xs) ys))))
  (if (null? xs)
   xs
  (let ((x (car xs)))
    (if (symbol? x)
      (cons x (up (cdr xs)))
      (concat x (up (cdr xs)))))))

;1.27
(define (flatten xs)
  (define (flatten-acc xs ys)
    (if (null? xs)
      ys
      (let ((x (car xs)))
        (if (symbol? x)
          (cons x (flatten-acc (cdr xs) ys))
          (flatten-acc x (flatten-acc (cdr xs) ys))))))
  (flatten-acc xs '()))

;1.28
(define (merge xs ys)
  (cond
    ((null? xs) ys)
    ((null? ys) xs)
    (else
     (let ((x (car xs))
           (y (car ys)))
       (if (< x y)
         (cons x (merge (cdr xs) ys))
         (cons y (merge xs (cdr ys))))))))

;1.29
(define (sort xs)
  (define (merge-pairs xs)
    (if (< (length xs) 2)
      xs
      (cons (merge (car xs) (cadr xs))
            (merge-pairs (cddr xs)))))
  (define (sort-lists xs)
    (cond
      ((null? xs) xs)
      ((= 1 (length xs)) (car xs))
      (else (sort-lists (merge-pairs xs)))))
  (sort-lists (down xs)))

;1.30
(define (sort/predicate pred xs)
  (define (merge xs ys)
    (cond
      ((null? xs) ys)
      ((null? ys) xs)
      (else
       (let ((x (car xs))
             (y (car ys)))
         (if (pred x y)
           (cons x (merge (cdr xs) ys))
           (cons y (merge xs (cdr ys))))))))
  (define (merge-pairs xs)
    (if (< (length xs) 2)
      xs
      (cons (merge (car xs) (cadr xs))
            (merge-pairs (cddr xs)))))
  (define (sort-lists xs)
    (cond
      ((null? xs) xs)
      ((= 1 (length xs)) (car xs))
      (else (sort-lists (merge-pairs xs)))))
  (sort-lists (down xs)))

;1.31
(define (leaf n)
  n)
(define (interior-node s b1 b2)
  (list s b1 b2))
(define leaf? number?)
(define lson cadr)
(define rson caddr)
(define (contents-of b)
  (define (contents-of-acc b ns)
    (if (leaf? b)
      (cons b ns)
      (contents-of-acc (cadr b) (contents-of-acc (caddr b) ns))))
  (contents-of-acc b '()))

;1.32
(define (double-tree b)
  (if (leaf? b)
    (* b 2)
    (list (car b) (double-tree (lson b)) (double-tree (rson b)))))

;1.33
(define my-tree
  (interior-node 'red
    (interior-node 'bar
      (leaf 26)
      (leaf 12))
    (interior-node 'red
      (leaf 11)
      (interior-node 'quux
        (leaf 117)
        (leaf 14)))))

(define (mark-leaves-with-red-depth b)
  (define (mark-with-count b n)
    (if (leaf? b)
      n
      (let ((m (+ n
                  (if (eq? (car b) 'red) 1 0))))
        (list (car b)
              (mark-with-count (lson b) m)
              (mark-with-count (rson b) m)))))
  (mark-with-count b 0))

;1.34
(define (path n b)
  (let ((m (car b)))
    (cond
      ((< n m) (cons 'left (path n (lson b))))
      ((> n m) (cons 'right (path n (rson b))))
      (else '()))))


;1.35
(define (number-leaves b)
  (define (number-leaves-n b n)
    (if (leaf? b)
      (list n (inc n))
      (let ((left (number-leaves-n (lson b) n)))
        (let ((right (number-leaves-n (rson b) (cadr left))))
          (list (list (car b) (car left) (car right))
                (cadr right))))))
  (number-leaves-n b 0))

;1.36
(define (g x ys)
  (define (inc-first xs)
    (cons (inc (car xs)) (cdr xs)))
  (define (inc-all-firsts xs)
    (if (null? xs)
      xs
      (cons (inc-first (car xs)) (inc-all-firsts (cdr xs)))))
  (cons x (inc-all-firsts ys)))

(define (number-elements xs)
  (if (null? xs)
    xs
    (g (list 0 (car xs)) (number-elements (cdr xs)))))
