#lang racket
(require rnrs/mutable-pairs-6)

(define (parallel-execute . procs)
  (map thread-wait
       (map (lambda (proc) (thread proc))
            procs)))

(define (test-and-set! cell)
  (if (mcar cell)
      true
      (begin (set-mcar! cell true)
             false)))

(define (clear! cell) (set-car! cell false))

(define (make-mutex)
  (define (make-test-cell) (mcons #f '()))
  (let ((cell (make-test-cell)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)
                 (begin (display "false")
                        #f))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (make-semaphore n)
  (let ((cell (mcons n '()))
        (aquire-mutex (make-mutex))
        (release-mutex (make-mutex)))
    (define (aquire)
      (aquire-mutex 'aquire)
      (let ((count (mcar cell)))
        (if (> count 0)
            (begin (set-car! cell (- count 1))
                   (aquire-mutex 'release)
                   (displayln (list "count:" count)))
            "stay locked")))
    (define (release)
      (release-mutex 'aquire)
      (let ((count (mcar count)))
        (if (< count n)
            (begin (set-car! cell (+ 1 (mcar cell)))
                   (aquire-mutex 'release)
                   (release-mutex 'release))
            'fullcount)))
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
         (make-serializer)))
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


(define serializer-test (make-serializer))
(define semaphore (make-semaphore 2))
(define shared-1 (mcons 0 '()))

(define (named a-shared n) (set-mcar! a-shared (+ n (mcar a-shared))))
(define mutex (make-mutex))
(mutex 'aquire)
(mutex 'aquire)
(mutex 'aquire)

((semaphore 'aquire))
((semaphore 'aquire))
((semaphore 'aquire))
((semaphore 'aquire))
((semaphore 'aquire))

(mcar shared-1)

(define (deposit account amount)
  (let ((s (account 'serializer))
        (d (account 'deposit)))
    (((s 'aquire) d) amount)
    ((s 'release))))

(define (withdraw account amount)
  (let ((s (account 'serializer))
        (w (account 'withdraw)))
    (s 'aquire)
    (w amount)
    (s 'release)))

(define (balance account)
  (account 'balance))
(define s (make-serializer))
(define cell (mcons 0 '()))
(define counter
  (lambda () (set-mcar! cell (+ 1 (mcar cell)))))

(define scounter (s counter))
;tests
(define a1 (make-account-and-serializer 1000000000))
(define a2 (make-account-and-serializer 1000000000))
(define (repeat n)
  (if (= n 0)
      'done
      (begin
      (parallel-execute
       (lambda () (scounter))
       (lambda () (scounter))
       (lambda () (scounter))
       (lambda () (scounter))
       (lambda () (scounter))
       (lambda () (scounter))
       (lambda () (scounter))
       (lambda () (scounter))
       )
      (repeat (- n 1)))
      ))
;(repeat 10)
(balance a1)
(mcar cell)
