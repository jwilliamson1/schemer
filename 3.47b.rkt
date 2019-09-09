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

(define (make-semaphore n)
  (define (make-test-cell) (mcons #f '()))
  (let ((count-cell (mcons n '()))
        (aquire-cell (make-test-cell))
        (release-cell (make-test-cell)))
    (define (aquire)
      (if (test-and-set! aquire-cell)
          (aquire)
          (let ((count (mcar count-cell)))
            (set-car! count-cell (- count 1))
            (if (> (mcar count-cell) 0)
                (begin 
                  (displayln (list "count:" (mcar count-cell)))
                  (clear! aquire-cell))
                "all busy"))))
    (define (release)
      (if (test-and-set! release-cell)
          (release)
          (let ((count (mcar count-cell)))
            (if (< count n)
                (begin (set-car! count-cell (+ 1 (mcar count-cell)))
                       (clear! aquire-cell)
                       (clear! release-cell))
                'fullcount))))
    (define (dispatch m)
      (cond ((eq? m 'aquire) aquire)
            ((eq? m 'release) release)
            (else "no opt")))
    dispatch))
    

(define (make-serializer)
  (let ((mutex (make-semaphore 0)))
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

((semaphore 'aquire))
((semaphore 'aquire))
((semaphore 'release))
((semaphore 'aquire))
((semaphore 'release))
((semaphore 'aquire))
((semaphore 'release))
(display "got here")
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
(repeat 10)
(balance a1)
(mcar cell)
