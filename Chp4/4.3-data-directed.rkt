#lang racket
(define *the-table* (make-hash))
(define (put key value)(hash-set! *the-table* key value))
(define (get key)(hash-ref *the-table* key #f))

(define (type-tag exp)
  (car exp))


(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; specific implementations
(define (lookup-variable-value)(error "not implemented"))
(define (text-of-quotation exp)
  (cadr exp))
;assignment
(define (assignment-variable exp) 
  (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (set-variable-value! var val env)(error "not done"))
(define (eval-assignment exp env)
  (set-variable-value! 
   (assignment-variable exp)
   (eval (assignment-value exp) env)
   env)
  'ok)
;define
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda 
       (cdadr exp)   ; formal parameters
       (cddr exp)))) ; body
(define (eval-definition exp env)
  (define-variable! 
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)
;lambda
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
(define (eval-procedure exp env) (make-procedure 
          (lambda-parameters exp)
          (lambda-body exp)
          env))

(define (install-eval-actions)
  (put 'self-evaluating (lambda (x y) x))
  (put 'variable (lambda (x y)(lookup-variable-value)))
  (put 'quote (lambda (x y)(text-of-quotation x)))
  (put 'set! (lambda (x y) (eval-assignment x y)))
  (put 'define (lambda (x y) eval-definition x y))
  (put 'lambda (lambda (x y) eval-procedure x y))
  'Done)

(install-eval-actions)

(define (eval exp env)
  (cond ((self-evaluating? exp) 
         exp)
        ((variable? exp) 
         (lookup-variable-value exp env))
        (else
         (let ((op (get (type-tag exp))))
          (if op
              (op exp env)
              (error "operation not found"))))))

(eval 1 (list '()))
(eval "echo" (list '()))
(eval (list 'quote 'text 'more) (list '()))
