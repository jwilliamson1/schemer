#lang racket
(define atom?
  (lambda (x)
    (and (not (null? x))
         (not (pair? x)))))

(define sero?
  (lambda (x)
    (null? x)))

(sero? '())

(define udd1
  (lambda (n)
    (cons '() n)))

(udd1 '())

(define zub1
  (lambda (n)
    (cdr n)))

(zub1(udd1 (udd1 '())))

(define edd
  (lambda (n m)
    (cond ((sero? n) m)
          ((sero? m) n)
          (else (edd (zub1 n) (udd1 m))))))
(define two (udd1 (udd1 '())))
(define three (udd1 (udd1 (udd1 '()))))
(length two)
(length (edd two three))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t )
      (( atom? ( car l)) (lat? ( cdr l)))
      (else #f ))) ) 

(lat? '())
(lat? 'a)
(lat? '(a (b)))
(lat? '((a) b))
(lat? '(a b c))

(car 'a)