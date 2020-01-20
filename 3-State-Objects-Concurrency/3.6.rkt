#lang sicp
;"random" number generator
(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

(define x1 (rand-update 0))
(define x2 (rand-update x1))
(define x3 (rand-update x2))
(define x4 (rand-update x3))
(define x5 (rand-update x4))
x1
x2
x3
x4
x5
;function that returns a closure
;closures seem to be the way to initialize objects with state in scheme
(define (r)
  (let ((seed 0))
    (define (dispatch m)
      (cond ((eq? m 'reset) (lambda (x)(set! seed x)))
            ((eq? m 'generate) (begin (set! seed (rand-update seed))
                                      seed))
            (else error "invalid operation")))
    dispatch))
;define rand in terms of r and the closure it returns

;test that rand works first with no args assuming 0 as seed
(define rand (r ))
(rand 'generate)
(rand 'generate)
(rand 'generate)
;test that pattern reoccurs
((rand 'reset) 0)
(rand 'generate)
(rand 'generate)
(rand 'generate)
;test the rand works using a new seed
((rand 'reset) 42)
(rand 'generate)
(rand 'generate)
(rand 'generate)
;test the pattern reoccurs
((rand 'reset) 42)
(rand 'generate)
(rand 'generate)
(rand 'generate)