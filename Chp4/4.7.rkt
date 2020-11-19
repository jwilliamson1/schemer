#lang racket
(define *the-table* (make-hash))

(define (eval exp env)
  (cond ((self-evaluating? exp) 
         exp)
        ((variable? exp) 
         (lookup-variable-value exp env))
        ((quoted? exp) 
         (text-of-quotation exp))
        ((assignment? exp) 
         (eval-assignment exp env))
        ((definition? exp) 
         (eval-definition exp env))
        ((if? exp) 
         (eval-if exp env))
        ((lambda? exp)
         (make-procedure 
          (lambda-parameters exp)
          (lambda-body exp)
          env))
        ((let? exp)
         (eval (let->combination exp) env))
        ((begin? exp)
         (eval-sequence 
          (begin-actions exp) 
          env))
        ((cond? exp) 
         (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values 
                 (operands exp) 
                 env)))
        (else
         (error "Unknown expression 
                 type: EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure 
          procedure 
          arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters 
              procedure)
             arguments
             (procedure-environment 
              procedure))))
        (else
         (error "Unknown procedure 
                 type: APPLY" 
                procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((snd (list-of-values (rest-operands exps) env))
            (fst (eval (first-operand exps) env)))
      (cons fst snd))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) 
         (eval (first-exp exps) env))
        (else 
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) 
                        env))))

(define (lookup-variable-value)(error "not implemented"))

(define (eval-assignment exp env)
  (set-variable-value! 
   (assignment-variable exp)
   (eval (assignment-value exp) env)
   env)
  'ok)

(define (eval-definition exp env)
  (define-variable! 
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) 
  (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

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

(define (lambda? exp) 
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate 
                 consequent 
                 alternative)
  (list 'if 
        predicate 
        consequent 
        alternative))

(define (begin? exp) 
  (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) 
  (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) 
  (car clause))

(define (cond-actions clause) 
  (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (test-then-apply? exp)
  (eq? (car exp) '=>))

(define (recipient-func action-clause)
  (cdr action-clause))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false     ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp 
                 (cond-actions first))
                (error "ELSE clause isn't 
                        last: COND->IF"
                       clauses))
            (if (test-then-apply? (cond-actions first))
                (let ((pred-result (eval (cond-predicate first)))
                      (recipient (recipient-func (cond-actions first))))
                  (make-if pred-result
                           (recipient pred-result)
                           (expand-clauses
                            rest))))                           
                (make-if (cond-predicate first)
                     (sequence->exp 
                      (cond-actions first))
                     (expand-clauses 
                      rest))))))

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

(define ltest (make-let '(x y )'((+ 3 4) (- 3 2)) '(* x y)))
(let->combination ltest)
(let*->nested ltest)
