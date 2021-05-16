#lang racket
(define-syntax-rule (assert expr)
  (unless expr
    (error 'assert "assertion failed: ~s" (quote expr))))

(define-syntax-rule (noisy-v1 expr)
  (begin
    (printf "Evaluating: ~s\n" (quote expr))
    expr))

(define-syntax-rule (noisy-v2 expr)
  (begin
    (printf "Evaluating: ~s\n" (quote expr))
    (begin0
      expr
      (printf "done\n"))))

(define-syntax-rule (andlet1 var e1 e2)
  (let ([var e1])
    (if var e2 #f)))

(define-syntax-rule (iflet x e1 e2 e3)
  (let ([tmp e1])
    (if tmp
        (let ([x e1])
          e2)
        e3)))

(define alist '((1 . apple) (2 . pear)))
(equal? (iflet x (assoc 1 alist) (cdr x) 'none) 'apple)
(equal? (let ([x 'plum]) (iflet x (assoc 3 alist) (cdr x) x)) 'plum)

(define-syntax-rule (forever expr)
  (define (forever-iter)
    (begin
      expr
      (forever-iter expr)))
  (forever-iter))