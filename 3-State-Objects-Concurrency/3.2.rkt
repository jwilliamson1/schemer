#lang racket

(define (make-acc n)
  (λ (x) (begin (set! n (+ n x))
                n)))

(define (monitored f)
  (let ((count 0))
  (define (mf arg)
    (cond ((eq? arg 'how-many-calls?) count)
          ((eq? arg 'reset) (set! count 0))
          (else (begin (set! count (add1 count))
                  (f arg)))))
    mf))


(define s (monitored sqrt))

(s 100)
(s 64)
(s 'how-many-calls?)
(s 'reset)
(s 'how-many-calls?)
(s 100)
(s 64)
(s 'how-many-calls?)