(define (value-of-program timeslice pgm)
  (initialize-store!)
  (initialize-scheduler! timeslice)
  (cases program pgm
    (a-program (exp1)
      (value-of/k exp1 (init-env) (end-main-thread-cont)))))

(define (value-of/k exp env cont)
  (cases expression exp
    (const-exp (num)
      (apply-cont cont (num-val num)))
    (var-exp (var)
      (apply-cont cont (apply-env env var)))
    (proc-exp (var body)
      (apply-cont cont
        (proc-val (procedure var body env))))
    (letrec-exp (p-name b-var p-body letrec-body)
      (value-of/k letrec-body (extend-env-rec p-name b-var p-body env) cont))
    (zero?-exp (exp1)
      (value-of/k exp1 env (zero1-cont cont)))
    (let-exp (var exp1 body)
      (value-of/k exp1 env (let-exp-cont var body env cont)))
    (if-exp (exp1 exp2 exp3)
      (value-of/k exp1 env (if-test-cont exp2 exp3 env cont)))
    (diff-exp (exp1 exp2)
      (value-of/k exp1 env (diff1-cont exp2 env cont)))
    (call-exp (rator rand)
      (value-of/k rator env (rator-cont rand env cont)))
    (assign-exp (var exp1)
      (value-of/k exp1 env (assign-cont var env cont)))
    (spawn-exp (exp1)
      (value-of/k exp1 env (spawn-cont cont)))
    ))


(define (apply-cont cont val)
  (if (time-expired?)
    (begin
      (place-on-ready-queue!
        (lambda () (apply-cont cont val)))
      (run-next-thread))
    (begin
      (decrement-timer!)
      (cases continuation cont
        (end-cont ()
          (begin
            (eopl:printf "End of computation.~%")
            val))
        (zero1-cont (saved-cont)
          (apply-cont saved-cont
            (bool-val
              (zero? (expval->num val)))))
        (let-exp-cont (var body saved-env saved-cont)
          (value-of/k body (extend-env var val saved-env) saved-cont))
        (if-test-cont (exp2 exp3 saved-env saved-cont)
          (if (expval->bool val)
            (value-of/k exp2 saved-env saved-cont)
            (value-of/k exp3 saved-env saved-cont)))
        (diff1-cont (exp2 saved-env saved-cont)
          (value-of/k exp2 saved-env (diff2-cont val saved-cont)))
        (diff2-cont (val1 saved-cont)
          (apply-cont saved-cont
            (num-val (- (expval->num val1)
                        (expval->num val)))))
        (rator-cont (rand saved-env saved-cont)
          (value-of/k rand saved-env (rand-cont val saved-cont)))
        (rand-cont (val1 saved-cont)
          (apply-procedure/k (expval->proc val1) val saved-cont))
        (assign-cont (var saved-env saved-cont)
          (begin
            (setref! (apply-env saved-env var) val)
            (apply-cont saved-cont (num-val 27))))
        (spawn-cont (saved-cont)
          (begin
            (place-on-ready-queue!
              (lambda ()
                (apply-procedure/k
                  (expval->proc val)
                  (num-val 28)
                  (end-subthread-cont))))
            (apply-cont saved-cont (num-val 73))))
        (end-main-thread-cont ()
          (begin
            (set-final-answer! val)
            (run-next-thread)))
        (end-subthread-cont ()
          (run-next-thread))
  ))))

(define (apply-procedure proc1 val)
  (cases proc proc1
    (procedure (var body saved-env)
      (value-of body (extend-env var (newref val) saved-env)))))

; continuation

(define-datetype continuation continuation?
  (end-cont)
  (zero1-cont
    (saved-cont continuation?))
  (let-exp-cont
    (var identifier?)
    (body expression?)
    (saved-env environment?)
    (saved-cont continuation?))
  (if-test-cont
    (exp2 expression?)
    (exp3 expression?)
    (saved-env environment?)
    (saved-cont continuation?))
  (diff1-cont
    (exp2 expression?)
    (saved-env environment?)
    (saved-cont continuation?))
  (diff2-cont
    (val1 expval?)
    (saved-cont continuation?))
  (rator-cont
    (rand expression?)
    (saved-env environment?)
    (saved-cont continuation?))
  (rand-cont
    (val1 expval?)
    (saved-cont continuation?))
  (assign-cont
    (var identifier?)
    (saved-env environment?)
    (saved-cont continuation?))
  (spawn-cont
    (saved-cont continuation?))
  (end-main-thread-cont)
  (end-subthread-cont)
  )

; store
(define (empty-store)
  '())

(define the-store 'uninitialized)

(define (get-store)
  the-store)

(define (initialize-store!)
  (set! the-store (empty-store)))

(define (reference? v)
  (integer? v))

(define (newref val)
  (set! the-store (append the-store (list val)))
  (- (length the-store) 1))

(define (deref ref)
  (list-ref the-store ref))

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
        (setref-inner (cdr store1) (- ref1 1))))))
  (set! the-store (setref-inner the-store ref)))

; Scheduler

(define the-ready-queue '())
(define the-final-answer '())
(define the-max-time-slice '())
(define the-time-remaining '())

(define (initialize-scheduler ticks)
  (set! the-ready-queue (empty-queue))
  (set! the-final-answer 'uninitialized)
  (set! the-max-time-slice ticks)
  (set! the-time-remaining the-max-time-slice))

(define (place-on-ready-queue! thread1)
  (set! the-ready-queue (enqueue the-ready-queue thread1)))

(define (run-next-thread)
  (if (empty? the-ready-queue)
    the-final-answer
    (dequeue the-ready-queue
      (lambda (first-ready-thread other-ready-threads)
        (set! the-ready-queue other-ready-threads)
        (set! the-time-remaining the-max-time-slice)
        (first-ready-thread)))))

(define (set-final-answer! val)
  (set! the-final-answer val))

(define (time-expired?)
  (zero? the-time-remaining))

(define (decrement-timer!)
  (set! the-time-remaining 
    (- the-time-remaining 1)))

;mutex
(define-datetype mutex mutex?
  (a-mutex
    (ref-to-closed? reference?)
    (ref-to-wait-queue reference?)))
