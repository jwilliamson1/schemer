#lang racket
(define (make-account balance pass-origin)
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
            ((eq? m 'make-joint) make-joint)
            ((eq? m 'balance) (displayln (list "Your balance: " balance)))
            (else (error "Unknown request: 
                 MAKE-ACCOUNT" m))))
    
    (define (locked) (displayln "call the cops"))

    (define (incorrect-pw)
      (begin (set! attempts (add1 attempts))
                    (if (> attempts 6) (locked)
                        (error "incorrect pw"))))

    (define (make-joint additional-pw)
      (dispatch additional-pw))
          
    (define (dispatch password)
      (lambda (trans-type pw)             
        (cond ((> attempts 6) (locked))
              ((not (eq? pw password)) (incorrect-pw))
              (else (select-transaction trans-type)))))
    (dispatch pass-origin)))

(define (make-joint acc orig-pass new-pass)
  ((acc 'make-joint orig-pass) new-pass))

(define a1 (make-account 100 'snarf))

((a1 'deposit 'snarf) 20)
((a1 'withdraw 'snarf) 50)
(a1 'balance 'snarf)

(define p1 (make-joint a1 'snarf 'rad))
(displayln "should be 110")
((p1 'deposit 'rad) 40)

(a1 'balance 'snarf)
(p1 'balance 'rad)

((a1 'deposit 'snarf) 80)
(a1 'balance 'snarf)
(p1 'balance 'rad)

((a1 'withdraw 'snarf) 50)

((a1 'deposit 'snarf) 20)
((a1 'withdraw 'snarf) 50)
(a1 'balance 'snarf)