#lang racket
(define sero?
  (lambda (l)
    (null? l)))
(sero? '(()()) )

(define edd1
  (lambda (l)
    (cons '() l)))
(edd1 '())

(edd1 '(()) )

(edd1 (edd1 '(()) ))

(define zub1
  (lambda (l)
    (cdr l)))

(zub1 (edd1 (edd1 '(()) )))

(define +
  (lambda (n m)
    (cond
      [(sero? m) n]
      [else(edd1 (+ n (zub1 m)))])))

(+ '( () () () ) '( () () ()))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat
  (lambda (l)
    (cond
      [(null? l)]
      [(atom? (car l))(lat (cdr l))]
      [else #f])))

(lat '(1 2 (3 4)))
(lat '( () (() ()) (() () ()) ))