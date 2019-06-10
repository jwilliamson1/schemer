#lang racket
(define (inverter input output)
  (define (invert-input)
    (let ((new-value 
           (logical-not (get-signal input))))
      (after-delay 
       inverter-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (logical-and s1 s2)
  (and (= s1 1)
       (= s2 1)))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) 
                        (get-signal a2))))
      (after-delay 
       and-gate-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

;(define (logical-or s1 s2)
 ; (or (= s1 1)
  ;     (= s2 1)))

(define (or-gate a b out)
  (let ((c  (make-wire)) (d (make-wire))(and-out (make-wire))(out (make-wire)))
    (inverter a c)
    (inverter b d)
    (and-gate c d out)
    (inverter and-out out)
    'ok))

;delay 2 * inverter delay + 1 and-gate delay (if parallel execution)


(define (ripple-carry-adder a b c-in sum carry)
  (iter n