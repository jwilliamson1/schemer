#lang racket
(require ffi/unsafe/atomic)
(require rnrs/mutable-pairs-6)

(define (test-and-set!-interleave cell)
  (if (mcar cell)
      true
      (begin (set-mcar! cell true)
             false)))

(define (test-and-set! cell)
  (start-atomic)
   (lambda () 
     (if (car cell)
         true
         (begin (set-car! cell true)
                false)))
  (end-atomic))

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

(define (deposit account amount)
  (let ((s (account 'serializer))
        (d (account 'deposit)))
    ((s d) amount)))

(define (withdraw account amount)
  (let ((s (account 'serializer))
        (w (account 'withdraw)))
    ((s w) amount)))

(define (balance account)
  (account 'balance))

;tests
(define a1 (make-account-and-serializer 1000000000))
(define a2 (make-account-and-serializer 1000000000))

;(withdraw a1 1)
;(withdraw a2 2)

(define (test n)
       (if (> n 0)
       (begin
         (- n 1)
         (withdraw a1 1)
         (withdraw a2 2))
       'done))

(test 20)