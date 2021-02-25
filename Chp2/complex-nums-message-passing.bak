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

(apply-generic 'real-part (make-from-real-imag 1 8))
(apply-generic 'imag-part (make-from-real-imag 1 8))
(apply-generic 'magnitude (make-from-real-imag 1 8))
(apply-generic 'angle (make-from-real-imag 1 8))

(define m (apply-generic 'magnitude (make-from-real-imag 1 8)))
(define a (apply-generic 'angle (make-from-real-imag 1 8)))

(apply-generic 'real-part (make-from-mag-ang m a))
(apply-generic 'imag-part (make-from-mag-ang m a))