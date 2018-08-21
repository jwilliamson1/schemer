#lang racket
(define (make-account balance pw)  
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m p)
    (if (eq? p pw)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: 
                 MAKE-ACCOUNT" m)))
        (error "incorrect pw")))
  dispatch)

(define a1 (make-account 100 'snarf))

((a1 'deposit 'snarf) 20)
((a1 'withdraw 'snarf) 50)