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

(define (named-let? let-block)
  (symbol? (cadr let-block)))

(define (let-name let-block) (car let-block))

(define (make-begin seq) (cons 'begin seq))

(define (make-definition label value)(list 'define label value))

(define (let->combination exp) 
  (let* ((label (cadr exp))
         (let-block (if (named-let? exp)(cdr exp) exp))
         (vars (let-variables let-block))
         (exps (let-expressions let-block))
         (body (let-body let-block)))
    (cond ((null? vars) error "No variables in let block")
          ((null? exps) error "No expressions in let block")
          ((null? body) error "No body in let block")
          (else (if (named-let? exp)
                    (make-lambda '() ; no params
                                 (list 
                                  (list (make-definition label 
                                                         (make-lambda vars body))
                                        (cons (make-lambda vars body) exps))))
                    (cons (make-lambda vars body) exps))))))

(define let1 '(let ((x (+ 5 5)))(+ x 1)))

(define let2 '(let ((x (+ 1 2))
                    (y (* x 2)))
                (+ 1 x y)))

(define let3 '(let* ((w 3)
                     (x w)
                     (y (+ x 2))
                     (z (+ x y 5)))
                (* x z)))

(define (last-var? vars) (null? (cdr vars)))

(define (let*->nested let-block)
  (define (let*-iter real-body vars exps)
    (if (null? vars)
        (list (list (make-lambda vars real-body)))
        (list (list (make-lambda (list (car vars))
                                 (let*-iter real-body (cdr vars) (cdr exps)))
                    (car exps)))))
  (car (let*-iter
        (let-body let-block)
        (let-variables let-block)
        (let-expressions let-block))))

(let*->nested let3)

(define (make-let list-of-vars list-of-expressions body)
  (let ((list-wrap (map list list-of-expressions)))
    (cons 'let (list (map cons list-of-vars list-wrap) body))))

(define (make-single-let list-of-vars list-of-expressions body)
  (list 'let (list (list (car list-of-vars) (car list-of-expressions))) body))

(define ltest (make-let '(x y )'((+ 3 4) (- 3 2)) '(* x y)))
(let->combination ltest)
(let*->nested ltest)

(define (let*->nested-lets let-block)
  (define (let*-iter real-body vars exps)
    (if (last-var? vars)
        (make-single-let vars exps (car real-body))
        (make-single-let vars exps (let*-iter real-body (cdr vars) (cdr exps)))))
  (let*-iter (let-body let-block)
             (let-variables let-block)
             (let-expressions let-block)))
(newline)
(let*->nested-lets let3)
(newline)
(let->combination (let*->nested-lets let3))
(newline)
(let*->nested-lets ltest)
(newline)
(let->combination (let*->nested-lets ltest))
; Test named let
(define fib-iter-body '(let fib-iter ((a 1) (b 0) (count n))
                         (if (= count 0)
                             b
                             (fib-iter (+ a b) 
                                       a 
                                       (- count 1)))))

(let->combination fib-iter-body)