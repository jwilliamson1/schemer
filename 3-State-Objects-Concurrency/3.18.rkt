#lang sicp
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

(define z (make-cycle (list 'a 'b 'c)))

;(last-pair z)

(define (contains-cycle? lst)
  (let ((seen '()))
    (define (iter lst)
      (if (null? lst) #f
          (if (memq (car lst) lst) #t
              (if (pair? (car lst))
                  (begin
                    (set! seen (cons (car lst) seen))
                    (iter (cdr lst)))
                  (iter (cdr lst))))))
    (iter lst)))

(contains-cycle? z)

(define reused (cons 'x 'x))

(define four (cons 'a (cons reused reused)))

(define reused4 (cons 'x 'x))

(define reused2 (cons reused4 reused4))

(define seven (cons reused2 reused2))

(set-cdr! reused4 seven)
;never returns!

(define lst (list reused reused4 reused2))

(define never-used (cons 'a 'a))

(contains-cycle? reused)
(contains-cycle? four)