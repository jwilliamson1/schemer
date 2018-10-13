#lang sicp

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))


;(atom? 'a)
;(atom? 1)
;(atom? '())
;(atom? '(1))
;(atom? '(1 2))

(define eqan?
(lambda ( a1 a2)
(cond
((and (number? a1) (number? a2))
(= a1 a2))
((or (number? a1) (number? a2))
#f)
(else ( eq? a1 a2)))))
(display "equan? ")
(eqan? 1 '1)

(define (eqlist l1 l2)
  (cond
    ((and (null? l1)(atom? l2)) #f)
    ((and (atom? l1)(null? l2)) #f)
    ((and (pair? l1)(atom? l2)) #f)
    ((and (atom? l1)(pair? l2)) #f)
    ((and (null? l1)(pair? l2)) #f)
    ((and (pair? l1)(pair? l2))
     (and (eqlist (car l1) (car l2))
          (eqlist (cdr l1) (cdr l2))))
    (else (eq? l1 l2))))

(eqlist '(((test)) test) '(((test)) test))
(eqlist '() 'a)
(eqlist '() '())
(eqlist '(a) '())
(eqlist 'a 'b)
(eqlist 'a '(a))
(eqlist 'a '())
(eqlist 'a 'a)
(eqlist '(a v) '())
(eqlist '(a b) '(a b))
(eqlist '(strawberry ice cream) '(strawberry ice cream))
(eqlist '1 1)
