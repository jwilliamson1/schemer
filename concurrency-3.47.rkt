#lang racket
(require ffi/unsafe/atomic)
(require rnrs/mutable-pairs-6)

(define (parallel-execute . procs)
  (map thread-wait
       (map (lambda (proc) (thread proc))
            procs)))

(define (test-and-set!-interleave cell)
  (if (mcar cell)
      true
      (begin (set-mcar! cell true)
             false)))

(define (make-mutex)
  (let ((cell (mcons #f '())))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set!-interleave cell)
                 (the-mutex 'acquire)
                 'done)) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell) (set-car! cell false))

(define (make-semaphore n)
  (let ((cell (mcons n '()))
        (mutex (make-mutex)))
    (define (aquire)
      (mutex 'aquire)
      (let ((count (mcar count)))
        (if (> count 0)
            (set-car! cell (- count 1))
            (error "count below zero"))))
    (define (release)      
      (set-car! cell (+ 1 (mcar cell)))
      (mutex 'release))
    (define (dispatch m)
      (cond ((eq? m 'aquire) aquire)
            ((eq? m 'release) release)
            (else "no opt")))
    dispatch))
    

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))
            
(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin 
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer 
         (make-semaphore 8)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) 
             balance-serializer)
            (else (error "Unknown request: 
                          MAKE-ACCOUNT"
                         m))))
    dispatch))

(define semaphore-test (make-semaphore 2))
(define serializer-test (make-serializer))
(define shared-1 (mcons 0 '()))

(define (named a-shared n) (set-mcar! a-shared (+ n (mcar a-shared))))
(((semaphore-test 'aquire) named) shared-1 3)
(((semaphore-test 'aquire) named) shared-1 3)
((semaphore-test 'release))
(((semaphore-test 'aquire) named) shared-1 4)
((semaphore-test 'release))
;((serializer-test named) shared-1 2)
 
(mcar shared-1)

(define (deposit account amount)
  (let ((s (account 'serializer))
        (d (account 'deposit)))
    (((s 'aquire) d) amount)
    ((s 'release))))

(define (withdraw account amount)
  (let ((s (account 'serializer))
        (w (account 'withdraw)))
    (((s 'aquire) w) amount)
    ((s 'release))))

(define (balance account)
  (account 'balance))

;tests
(define a1 (make-account-and-serializer 1000000000))
(define a2 (make-account-and-serializer 1000000000))

(parallel-execute
 (lambda () (withdraw a1 1))
 (lambda () (withdraw a1 2))
 (lambda () (withdraw a1 3))
 (lambda () (withdraw a1 4))
 (lambda () (withdraw a1 1))
 (lambda () (withdraw a1 2))
 (lambda () (withdraw a1 3))
 (lambda () (withdraw a1 4))
 )

(balance a1)