#lang racket
(define (square x)
  (* x x))

(define (dis text)  
  (display text)
  (newline))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (map p seq)
  (accumulate (lambda (x y)(p x (p y))) 
              null seq))

(accumulate + 0 (list 1 2 4))

(map * (list 1 2 4))