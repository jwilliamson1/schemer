#lang racket
(define (make-account balance password)
  (let ((attempts 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance 
                       (- balance amount))
                 balance)
          "Insufficient funds"))
    
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    
    (define (select-transaction m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request: 
                 MAKE-ACCOUNT" m))))
    
    (define (locked) (displayln "call the cops"))

    (define (incorrect-pw)
      (begin (set! attempts (add1 attempts))
                    (if (> attempts 6) (locked)
                        (displayln "incorrect pw"))))
          
    (define (dispatch trans-type pw)
      (cond ((> attempts 6) (λ (x)(locked)))
            ((not (eq? pw password)) (λ (x)(incorrect-pw)))
            (else (select-transaction trans-type))))
    dispatch))

(define a1 (make-account 100 'snarf))

((a1 'deposit 'snarf) 20)
((a1 'withdraw 'snarf) 50)

((a1 'withdraw 'snarfs) 50)
((a1 'withdraw 'snarfs) 50)
((a1 'withdraw 'snarfs) 50)
((a1 'withdraw 'snarfs) 50)
((a1 'withdraw 'snarfs) 50)
((a1 'withdraw 'snarfs) 50)
((a1 'withdraw 'snarfs) 50)
((a1 'withdraw 'snarfs) 50)


