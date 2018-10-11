#lang sicp

;does structure contain cycle?

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)


(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z1 (make-cycle (list 'a 'b 'c 'a)))
(define z2 (list (list 'a 'b 'c)(list 'a 'b 'c)(list 'a 'b 'c)))
(define z3 (list 'a ))
(append! (last-pair z3) z3)
z1
z2
z3

(define (contains-cycle? x)
  (define (iter x seen)
    (cond ((null? (cdr x)) #f)
          ((memq x seen) #t)
          (else
           (begin
             (set! seen (cons x seen))
             (or (if (pair? (car x))
                     (iter (car x) seen)
                     #f)
                 (iter (cdr x) seen))))))
  (iter x '()))


(contains-cycle? z1)
(contains-cycle? z2)
(contains-cycle? z3)