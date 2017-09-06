#lang racket


(define allCols '(client initiative rebator))
;
;(define (tier-list list len)
;  (cond
;    ((cons (take len)
(define (take n list)
  (cond
    ((eq? list null)(quote()))
    ((= n 0)(quote()))
    (else(cons (car list)(take (- n 1) (cdr list))))))

(define (add1 n)
  (+ n 1))

(define (len list)
  (cond
    ((eq? list null)0)
    (else(add1 (len (cdr list))))))

(len allCols)

(take 3 allCols)

;(define (tier list)
;  (let ((n (len list)))
;    (cond
;      ((= n 0)(quote()))
;      (else(cons(take n list)(tier (cdr list)))))))
; implementation seems to be inefficient because it calculates length every time

(define (tier list)
  (define n(len list))
  (define (tier-helper n list)
    (cond
      ((= n 0)(quote()))
      (else(cons(take n list)(tier-helper (- n 1)(cdr list))))))
  (tier-helper n list))

(tier allCols)


(define (square n)(* n n))
    
(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))



