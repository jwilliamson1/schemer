#lang racket
(define A
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n)
               (A n (sub1 m)))))))

(A 2 1) ;5
(A 3 2) ;29

;(A 4 3) ;Keeps going; presumabley stops