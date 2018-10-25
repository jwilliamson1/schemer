#lang racket
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define new-entry build)

(define e1 (new-entry '(apples oranges bread)
                      '(pie juice crumbs)))

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car values))
      (else (lookup-in-entry-help name (cdr names)(cdr values) entry-f)))))

(lookup-in-entry 'arg e1 (lambda(x)(list "name not found: " x)))
(lookup-in-entry 'apples e1 (lambda(x)(list "name not found: " x)))
(lookup-in-entry 'bread e1 (lambda(x)(list "name not found: " x)))

(define extend-table cons)

(define e2 (new-entry '(appetizer entree beverage)
                                       '(pate boeuf vin)))
e1
e2
(define e3 (extend-table e1 '()))
(define e4 (extend-table e2 e3))
e4