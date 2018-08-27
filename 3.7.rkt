#lang racket
(define (make-account balance password)
  (let ((attempts 0)
        (passwords (cons password'())))
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
            ((eq? m 'make-joint) make-joint)
            ((eq? m 'balance) (displayln (list "Your balance: " balance)))
            (else (error "Unknown request: 
                 MAKE-ACCOUNT" m))))
    
    (define (locked) (displayln "call the cops"))

    (define (incorrect-pw)
      (begin (set! attempts (add1 attempts))
                    (if (> attempts 6) (locked)
                        (displayln "incorrect pw"))))

    (define (make-joint additional-pw)
      (begin (set! passwords (cons additional-pw passwords))
             dispatch))
          
    (define (dispatch trans-type pw)
      (cond ((> attempts 6) (λ (x)(locked)))
            ((not (member pw passwords)) (λ (x)(incorrect-pw)))
            (else (select-transaction trans-type))))
    dispatch))

(define a1 (make-account 100 'snarf))

(define (make-joint acc orig-pass new-pass)
  ((acc 'make-joint orig-pass) new-pass))

((a1 'deposit 'snarf) 20)
((a1 'withdraw 'snarf) 50)
(a1 'balance 'snarf)

(define p1 (make-joint a1 'snarf 'rad))
((p1 'deposit 'rad) 40)

(a1 'balance 'snarf)
(p1 'balance 'snarf)

((a1 'deposit 'snarf) 80)
(a1 'balance 'snarf)
(p1 'balance 'snarf)

((a1 'withdraw 'snarfs) 50)
((a1 'withdraw 'snarfs) 50)
((a1 'withdraw 'snarfs) 50)
((a1 'withdraw 'snarfs) 50)
((a1 'withdraw 'snarfs) 50)
((a1 'withdraw 'snarfs) 50)
((a1 'withdraw 'snarfs) 50)
((a1 'withdraw 'snarf) 50)