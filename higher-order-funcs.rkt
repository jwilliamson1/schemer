#lang racket

(define mip
  (lambda (p l)
    (cond
     ((null? l) (quote()))
      ((cons (p (car l))
                (mip p (cdr l)))))))

(mip (lambda (x)(+ 10 x))'(1 2 3))
(mip (lambda (x)(/ 2 x))'(1 2 3))

(define red
  (lambda (p l)
    (cond
      ((null? l)(quote()))
      ((p (car l)) 
       (cons (car l)(red p (cdr l))))
      (else (red p (cdr l))))))

(red (λ(x)(= x 2))'(1 2 3 4 2 6 12 15))
(red (λ(x)(> x 10))'(1 2 3 4 2 6 12 15))

(define make-pair
  (λ (x y)
    (cons x (cons y '()))))

(make-pair 5 6)

(define x-coord
  (λ (x)
    (car x)))

(x-coord(make-pair 5 6))

(define y-coord
  (λ (y)
    (car (cdr y))))

(y-coord(make-pair 5 6))

(define (deriv exp var)
  (cond
    ((constant? exp var) 0)
    ((same-var? exp var) 1)
    ((sum? exp)
     (make-sum(deriv (a1 exp) var)
              (deriv (a2 exp) var)))
    ((product? exp)
     (make-sum
      (make-product (m1 exp)
                    (deriv (m2 exp)var))
      