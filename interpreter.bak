#lang racket
(define atom?
  (lambda (x)
    (not (or (pair? x)(null? x)))))

(define first
  (lambda (x)
    (car x)))

(define second
  (lambda (x)
    (car (cdr x))))

(define third
  (lambda (x)
    (car (cdr (cdr x)))))

(second '(a b))

(atom? 'a)
(atom? null)
(atom? '(a b))

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

(define lookup-in-table 
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (lookup-in-entry name (car table)
                             (lambda(name)(lookup-in-table name (cdr table) table-f)))))))

(lookup-in-table 'apples e4 (lambda (x)x))

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? ( car e))
       (cond
         ((eq? (car e) (quote quote))*quote)
         ((eq? (car e) (quote lambda))*lambda)
         ((eq? (car e) (quote cond))*cond)
         (else *application)))
      (else *application))))

(define value
  (lambda (e)
    (meaning e (quote ()))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda ( e table)
    (cond
      ((number? e) e)
      (( eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car (quote ()))))

(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
            (cons table (cdr e)))))

(define table-of first)

(define formals-of second)

(define body-of third)

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines))
                table))
      ((meaning (question-of (car lines))
                table)
       (meaning (answer-of (car lines))
                table))
      (else (evcon (cdr lines) table)))))

(define else?
  (lambda (question)
    (cond
      ((atom? question)(eq? question (quote else)))
      (else #f))))

(define question-of first)

(define question-of second)

