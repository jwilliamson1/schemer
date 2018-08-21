#lang racket


(define (make-acc n)
  (λ (x) (begin (set! n (+ n x))
                n)))

(define A(make-acc 5))
(A 2)
(A 6)

