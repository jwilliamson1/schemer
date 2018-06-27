#lang racket
(define (square x)(* x x))
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op: 
            MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (make-from-mag-ang m a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) m)
          ((eq? op 'angle) a)
          ((eq? op 'real-part)
           (* m (cos a)))
          ((eq? op 'imag-part)
           (* m (sin a)))
          (else
           (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))
(displayln "make from real imag")
(apply-generic 'real-part (make-from-real-imag 1 8))
(apply-generic 'imag-part (make-from-real-imag 1 8))
(apply-generic 'magnitude (make-from-real-imag 1 8))
(apply-generic 'angle (make-from-real-imag 1 8))
(displayln "make from mag ang")
(apply-generic 'real-part (make-from-mag-ang 1 8))
(apply-generic 'imag-part (make-from-mag-ang 1 8))
(apply-generic 'magnitude (make-from-mag-ang 1 8))
(apply-generic 'angle (make-from-mag-ang 1 8))

(define m (apply-generic 'magnitude (make-from-real-imag 1 8)))
(define a (apply-generic 'angle (make-from-real-imag 1 8)))

(apply-generic 'real-part (make-from-mag-ang m a))
(apply-generic 'imag-part (make-from-mag-ang m a))

;explicit dispatch
;the problem with explicit dispatch is that each system must know how to convert to another systems type and as new object are added each existing selector has to be updated to support the new type
;and the new type needs to be updated to support all the existing types; conversion can be costly vs just looking up the correct proc in a table
;further this requires selectors to be updated to check for every type and dispatch the correct selector
;this seems to introduce unnessary work when compared with data-directed programming
;not additive so name collisions could occur

;DATA DIRECTED PROGRAMMING
;this method prefers systems where new functions are being added since once a generic interface is developed to the primitives of the domain newer functionality can be more easily applied
;this requires generally only adding a new proc and adding to the install table for each object, and then providing access to that table function once externally.

;MESSAGE PASSING
;The advantage of message passing in a large system comes when you have many types with a similar interface, but different functionality
