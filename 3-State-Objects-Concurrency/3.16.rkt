#lang sicp

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(count-pairs (cons (cons 'a '())(cons 'b '())))

(define reused (cons 'x 'x))

(define four (cons 'a (cons reused reused)))

(count-pairs four)

(define reused4 (cons 'x 'x))

(define reused2 (cons reused4 reused4))

(define seven (cons reused2 reused2))

;(set-cdr! reused4 seven) never returns!

(count-pairs seven)



