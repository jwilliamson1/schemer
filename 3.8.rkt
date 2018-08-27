#lang racket
(define f
  (let ((state #f))
    (define (dispatch v)
      (if state 0
          (begin (set! state #t) v)))
    dispatch))


;(+ (f 0) (f 1))

(+ (f 1) (f 0))