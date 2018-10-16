#lang sicp
(apply + '(1 2 3))
(apply / '(1 2 3 4))
(apply sqrt '(25))
;(apply cons '(1 ()))
(define (mycons . orig-args)
  (define (iter args)
    (cond ((null? args) '())
          (else (cons (car args)(iter (cdr args))))))
  (iter orig-args))

(apply mycons '(1 2 3 4))