#lang sicp
(define (passing-set-test! value-to-mutate)
  (set! value-to-mutate 'different)
  value-to-mutate)

(define v1 'original)
v1
(passing-set-test! v1)
v1

(define v2 (cons 'original '()))
v2
(passing-set-test! v2)
v2

(define (set-car-test! value-to-mutate)
  (set-car! value-to-mutate 'different)
  value-to-mutate)

(set-car-test! v2)
v2

(define three (list 1 2 3))

(set-car-test! (cdr three))
three