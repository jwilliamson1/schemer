#lang racket
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (let? exp) 
  (tagged-list? exp 'let))

(define (let-assignments let-block)
  (cadr let-block))

(define (let-expressions let-block)
  (map cadr (let-assignments let-block)))

(define (let-variables let-block)
  (map car (let-assignments let-block)))

(define (let-body let-block)
  (cddr let-block))

(define (let->combination let-block)
  (let ((vars (let-variables let-block))
        (exps (let-expressions let-block))
        (body (let-body let-block)))
    (cond ((null? vars) error "No variables in let block")
          ((null? exps) error "No expressions in let block")
          ((null? body) error "No body in let block")
          (else (cons (make-lambda vars body) exps)))))

(define let1 '(let ((x (+ 5 5)))(+ x 1)))
(define let2 '(let ((x (+ free 2))(y (* free2 free3))) (+ 1 x y)))