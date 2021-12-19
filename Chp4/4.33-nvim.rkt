#lang sicp
(define (filter f xs)
  (if (null? xs) '()      
      (if (f (car xs))
          (cons (car xs)(filter f (cdr xs)))
          (filter f (cdr xs)))))

(define (displayln exp)
  (display exp)
  (newline))

(#%require (only racket/base make-hash))
(#%require (only racket/base hash-set!))
(#%require (only racket/base hash-ref))

(define *the-table* (make-hash));make THE table
(define (put key value) (hash-set! *the-table* key value))
(define (get key) (hash-ref *the-table* key))

(define (true? x)
  (not (eq? x false)))

(define (false? x)
   (eq? x false))

(define (eval exp env)
  (cond ((self-evaluating? exp) 
         exp)
        ((variable? exp) 
         (lookup-variable-value exp env))
        ((quoted? exp) 
         (text-of-quotation exp env))
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
        ((let*? exp)
         (eval (let*->nested exp) env))
        ((letrec? exp)
         (eval (letrec->let exp) env))
        ((begin? exp)
         (eval-sequence 
          (begin-actions exp) 
          env))
        ((cond? exp) 
         (eval (cond->if exp) env))
        ((for? exp)
         (eval (for->lambda exp) env))
        ((do? exp)
         (eval (do->lambda exp) env))
        ((while? exp)
         (eval (while->lambda exp) env))
        ((until? exp)
         (eval (until->while exp) env))
        ((not? exp)
         (eval (not->if exp) env))
        ((and? exp)
         (eval (and->if exp) env))
        ((or? exp)
         (eval (or->if exp) env))
        ((application? exp)
         (apply_ (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error "Unknown expression 
                 type: EVAL" (list exp env)))))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) 
  (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result
                (actual-value 
                 (thunk-exp obj)
                 (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           ;; replace exp with its value:
           (set-car! (cdr obj) result) 
           ;; forget unneeded env:
           (set-cdr! (cdr obj) '()) 
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (delay-it exp env)
  (list 'thunk exp env))
(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (apply_ procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values 
           arguments 
           env)))  ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args 
            arguments 
            env)   ; changed
           (procedure-environment procedure))))
        (else (error "Unknown procedure 
                      type: APPLY_" 
                     procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value 
             (first-operand exps) 
             env)
            (list-of-arg-values 
             (rest-operands exps)
             env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it 
             (first-operand exps) 
             env)
            (list-of-delayed-args 
             (rest-operands exps)
             env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) 
                           env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-cond exp env)
  (eval-cond-clauses (cond-clauses exp) env))
(define (eval-cond-clauses clauses env)
  (if (null? clauses)
      'false     ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp 
                 (cond-actions first))
                (error "ELSE clause isn't 
                        last: EVAL->COND"
                       clauses))
            (if (=>? (cdr first))
                (let ((test-val (eval (cond-predicate first) env)))
                  (if (true? test-val)
                           (apply_ (eval (cond-recipient first) env) (list test-val))
                           (eval-cond-clauses rest env)))
                  (eval (make-if (cond-predicate first)
                                 (sequence->exp 
                                  (cond-actions first))
                                 (eval-cond-clauses rest env)) env))))))


(define (eval-sequence exps env)
  (cond ((last-exp? exps) 
         (eval (first-exp exps) env))
        (else 
         (actual-value (first-exp exps) 
                       env)
         (eval-sequence (rest-exps exps) 
                        env))))

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

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define make-cons '(lambda (x y) (lambda (m) (m x y))))

(define (expand-list pairs)
  (if (null? pairs) ''()      
      (list 'cons (car pairs) (expand-list (cdr pairs)))))

(define (text-of-quotation exp env)
  (let ((quoted-value (cadr exp)))
    (if (not (pair? quoted-value))
        quoted-value
        (eval (expand-list quoted-value) env))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (unassignment? exp)
  (tagged-list? exp 'unset!))

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

(define (not? exp)
  (tagged-list? exp 'not))

(define (not->if exp)
  (make-if (cadr exp)
           'false
           'true))

(define (and? exp)
  (tagged-list? exp 'and))

(define (and->if preds)
  (if (null? (cdr preds))
      'true
      (make-if (cadr preds) (and->if (cdr preds))
               'false)))

;(and->if '(and (= 3 1)))
;(and->if '(and (= 2 2)(= 3 3)))

(define (or? exp)
  (tagged-list? exp 'or))

(define (or->if preds)
  (if (null? (cdr preds))
      'false
      (make-if (cadr preds) 'true
               (or->if (cdr preds)))))

;(or->if '(or false))
;(or->if '(or true true))

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
(define (=>? exp) 
  (tagged-list? exp '=>))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) 
  (car clause))
(define (cond-actions clause) 
  (cdr clause))
(define (cond-recipient clause) 
  (caddr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
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
            (if (=>? (cdr first))
                 (make-if (cond-predicate first)
                          (list (cond-recipient first) (cond-predicate first))
                          (expand-clauses 
                      rest))
                 (make-if (cond-predicate first)
                     (sequence->exp 
                      (cond-actions first))
                     (expand-clauses 
                      rest)))))))

; (define (conda)
;   (cond ((assoc 'a '((a 1)(b 2))) => cadr)
;         (else "woop")))
; 
; (define (condb)
;   (cond ((eq? 3 3) 'Three)
;         (else 'Wrong)))



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
          ((named-let? exp)
                    (list (make-lambda '() ; no params
                                 (list 
                                  (make-definition label 
                                                         (make-lambda vars body))
                                        (cons (make-lambda vars body) exps)))))
                    
          (else (cons (make-lambda vars body) exps)))))


; (define let1 (let ((x (+ 5 5)))(+ x 1)))
; 
; (define let2 (let ((x (+ 1 2))
;                     (y (* 3 2)))
;                 (+ 1 x y)))
; 
; (define let3 (let* ((w 3)
;                     (x w)
;                     (y (+ x 2))
;                     (z (+ x y 5)))
;                (display z)
;                (newline)
;                (display x)
;                (* x z)))


(define (last-var? vars) (null? (cdr vars)))

(define (let*? exp) 
  (tagged-list? exp 'let*))

(define (let*->nested let-block)
  (let ((var-defs (cadr let-block))
        (body (cddr let-block)))
  (expand-let* var-defs body)))
(define (expand-let* var-defs body)
  (if (null? (cdr var-defs))
      (make-let (car var-defs) (list (sequence->exp body)))
      (make-let (car var-defs) (list (expand-let* (cdr var-defs) body)))))

(define (for? exp) 
  (and (tagged-list? exp 'for)(not (null? (cdr exp)))))

(define (do? exp) 
  (and (tagged-list? exp 'do)(not (null? (cdr exp)))))

(define (while? exp) 
  (and (tagged-list? exp 'while)(not (null? (cdr exp)))))

(define (until? exp)
  (tagged-list? exp 'until))

(define (make-assignment var exp)
  (cons 'set! (cons var (cons exp '()))))

(define (for->lambda exp)
  (let ((binding (cadr exp))
        (check (caddr exp))
        (inc (cadddr exp))
        (body (car (cddddr exp))))
    (make-let binding
              (list (make-definition
                     'for
                     (make-lambda '()
                                  (list (make-if check                                         
                                                 (make-begin
                                                  (list body
                                                        (make-assignment (car binding) inc)
                                                        (list 'for)))
                                                 "done"))))
                    (list 'for)))))

(define (do->lambda exp)
  (let ((check (cadr exp))
        (body (caddr exp)))
    (make-begin
     (list (make-definition 'do
                            (make-lambda '()
                                         (list (make-begin
                                                (list body
                                                      (make-if check
                                                               (list 'do)
                                                               "done"))))))
     (list 'do)))))

;(do->lambda '(do (< x 20)(begin (display x)(set! x (+ x 2)))))
;(do->lambda '(do (< x 10)(begin (display x)(set! x (+ x 2)))))


(define (while->lambda exp)
  (let ((check (cadr exp))
        (body (caddr exp)))
    (make-begin
     (list (make-definition 'while
                            (make-lambda '()
                                         (list (make-if check
                                                        (make-begin
                                                         (list body
                                                               (list 'while)))
                                                        "done"))))
     (list 'while)))))

;(while->lambda '(while (< x 20)(begin (display x)(set! x (+ x 2)))))

;'(begin (define x 0)(while (< x 20)(begin (display x)(set! x (+ x 2)))))

(define (until->while exp)
  (let ((check (cadr exp))
        (body (caddr exp)))
    (while->lambda (list 'while (list 'not check)
          body))))

;(until->while '(until (< x 20)(begin (display x)(set! x (- x 2)))))

;'(begin (define x 50)(until (< x 20)(begin (display x)(set! x (- x 2)))))

(define (make-let var-defs body)
  (cons 'let (cons (list var-defs) body)))

; (define (make-let list-of-vars list-of-expressions body)
;   (let ((list-wrap (map list list-of-expressions)))
;   (cons 'let (list (map cons list-of-vars list-wrap) body))))


;(define ltest (make-let '(x y )'((+ 3 4) (- 3 2)) '(* x y)))
;(let->combination ltest)
;(let*->nested ltest)

;(for->lambda '(for (x 0)(< x 10)(+ x 1)(display x)))
;(for->lambda '(for (x 10)(> x 0)(- x 1)(display x)))
; 


(define (make-procedure parameters body env)  
  ;(displayln (list "normal body " body))
  ;(let ((maybe-scanned (scan-out body)))
    ;(displayln (list "maye scanned " maybe-scanned))
    (list 'procedure parameters body env));)
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (lazy-list? p)
  (tagged-list? p 'lazy-list))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (if (null? variables)
      (list (list '() '()))
      (map cons variables values)))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (car frame) (cdr frame)))
  (set-car! frame (cons var val)))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" 
                 vars 
                 vals)
          (error "Too few arguments supplied" 
                 vars 
                 vals))))

(define e1 (extend-environment '(a b) '(5 6) '()))
;e1

(define unassigned '*unassigned*)

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan bindings)
      (cond ((null? bindings)
             (env-loop 
              (enclosing-environment env)))
            ((eq? var (caar bindings))
             (let (( val (cdar bindings)))
               (if (eq? val unassigned)
                   (error "Cannot use variable before it is assigned: " var)
                   val)))
            (else (scan (cdr bindings)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan bindings)
      (cond ((null? bindings)
             (env-loop 
              (enclosing-environment env)))
            ((eq? var (caar bindings))
             (set-cdr! (car bindings) val))
            (else (scan (cdr bindings)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variablE: SET!" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan bindings)
      (cond ((null? bindings)
             (add-binding-to-frame! 
              var val frame))
            ((eq? var (caar bindings))
             (set-cdr! (car bindings) val))
            (else (scan (cdr bindings)))))
    (scan frame)))


;(define-variable! 'c 'oof e1)
;e1
;scan responsibilties keep state of frame iterate until null or match and do something in each case

(define (setup-environment)
  (let ((initial-env
         (extend-environment 
          (primitive-procedure-names)
          (primitive-procedure-objects)
          the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) 
  (cadr proc))

;4.21
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) 
         true 
         (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) 
         false 
         (ev? ev? od? (- n 1))))))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'assoc assoc)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '< <)
        (list '<= <=)
        (list '> >)
        (list '>= >=)
        (list 'cadr cadr)
        (list 'eq? eq?)
        (list 'newline newline)
        (list 'display display)
        (list 'runtime runtime)
        ;(list 'fib fib)
        ;(list 'fact factorial)
        ;(list 'f f)
        ;(list 'A A)
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) 
         (list 'primitive (cadr proc)))
       primitive-procedures))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define input-prompt  ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value 
                   input 
                   the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) 
  (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (case ((compound-procedure? object)
	      (display 
	       (list 'compound-procedure
		     (procedure-parameters object)
		     (procedure-body object)
		     '<procedure-env>)))
	((lazy-list? object)
		(display 'lazy-list))
	(else (display object))))

(define the-global-environment 
  (setup-environment))
(add-binding-to-frame! 'f 0 (first-frame the-global-environment))
; 4.16 pt 2
(define internal
  '((even? x)
    (define y (* x 2))
    
  (define (even? n)
    (if (= n 0)
        true
        (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0)
        false
        (even? (- n 1))))
  ))

; (define (int-test x)
;   (if (even? x) y x)
;   (define y (* x 2))
;     
;   (define (even? n)
;     (if (= n 0)
;         true
;         (odd? (- n 1))))
;   (define (odd? n)
;     (if (= n 0)
;         false
;         (even? (- n 1))))
;   )


;(lambda-body internal)
(define (get-internal-defs body)
  (filter (lambda (exp)(and (pair? exp)(eq? (car exp) 'define))) body))
(define (the-actual-body body)
  (car (filter (lambda (exp)(not (eq? (car exp) 'define))) body)))
(get-internal-defs internal)
(define (is-var? exp)
  (not (pair? (cadr exp))))
(define (procname-or-var exp)
  (if (is-var? exp) (cadr exp) (caadr exp)))
(define make-let-init (lambda (df) (list 'define (procname-or-var df) ''*unassigned*)))
(define let-inits (lambda (internal) (map make-let-init (get-internal-defs internal))))
(define (make-binding df)
  (if (is-var? df) (caddr df)
      (make-lambda (cdadr df) (cddr df))))
(define make-let-setval (lambda (df) (list 'set! (procname-or-var df) (make-binding df))))
(define let-setvals (lambda (internal) (map make-let-setval (get-internal-defs internal))))
(define (body-contains-defs def)
  (if (null? def) #f
      (if (and (pair? (car def)) (eq? (caar def) 'define)) #t
          (body-contains-defs (cdr def)))))
;(displayln "body-contains-defs")
;(body-contains-defs internal)
(define (scan-out body)
  (if (body-contains-defs body)
      ((lambda (thebody) (append                                 
                                  (append (let-inits thebody) (let-setvals thebody)) (list (the-actual-body thebody)))) body)
      body))
;(displayln "scanout")
;(scan-out internal)
;(scan-out '(* x x))
; (lambda ⟨vars⟩
;   (let ((u '*unassigned*)
;         (v '*unassigned*))
;     (set! u ⟨e1⟩)
;     (set! v ⟨e2⟩)
;     ⟨e3⟩))

(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec-bindings exp)(cadr exp))
(define (letrec-body exp)(cddr exp))
(define (binding-var binding) (car binding))
(define (binding-body binding) (cadr binding))
(define (make-letrec-its binding) (list (binding-var binding) ''*unassigned*))
(define (make-letrec-rebinds binding) (list 'set! (binding-var binding) (binding-body binding)))
(define (letrec->let exp)
  (let ((letrec-inits (map make-letrec-its (letrec-bindings exp)))
        (letrec-rebinds (map make-letrec-rebinds (letrec-bindings exp))))
        (append (list 'let letrec-inits) (append letrec-rebinds (letrec-body exp)))))
(define letrec1 '(letrec
      ((even?
        (lambda (n)
          (if (= n 0)
              true
              (odd? (- n 1)))))
       (odd?
        (lambda (n)
          (if (= n 0)
              false
              (even? (- n 1))))))
    (even? 5)))
(letrec->let letrec1)
; '(letrec
;       ((even?
;         (lambda (n)
;           (if (= n 0)
;               true
;               (odd? (- n 1)))))
;        (odd?
;         (lambda (n)
;           (if (= n 0)
;               false
;               (even? (- n 1))))))
;     (even? 5))
; ;e0 lambda vars-in-body
; ;eval letrec->let
; ;eval let->(lambda *unassigned *unassigned)
; ;creates e1
; ;even? *unassigned*
; ;odd? *unassigned
; ;set even and odd to lambdas
; ;eval f body
; 
; ;if let
; ;e0 lambda vars-in-body
; ;eval let
; ;eval let-> (lambda (l1 l2))
; ;eval of lambdas would create procs
; ; that don't have odd? or even? in their envs
; (define (f x)
;   (letrec
;       ((even?
;         (lambda (n)
;           (if (= n 0)
;               true
;               (odd? (- n 1)))))
;        (odd?
;         (lambda (n)
;           (if (= n 0)
;               false
;               (even? (- n 1))))))
;     (even? x)))
; 
; (define (factorial x)(letrec
;     ((fact
;       (lambda (n)
;         (if (= n 1)
;             1
;             (* n (fact (- n 1)))))))
;   (fact x)))
; 
; ((lambda (n)
;    ((lambda (fib) (fib fib n 0))
;     (lambda (ft k a)
;       (if (= k 1)
;           1
;           (* k (ft ft (- k 1)))))))
;  10)
; 
; ((lambda (n)
;    ((lambda (fib) (fib fib n))
;       (lambda (fb k)
;         (cond
;           ((< k 0) (error k))
;           ((= k 0) 0)
;           ((= k 1) 1)
;           (else (+ (fb fb (- k 1))(fb fb (- k 2))))))))
;  8)
; 
; 


(driver-loop)
; (define (conda)
;   (cond ((assoc 'a '((a 1)(b 2))) => cadr)
;         (else "woop")))
; 
; (define (condb)
;   (cond ((eq? 3 3) 'Three)
;         (else 'Wrong)))
; 
; (for (x 0)(< x 10)(+ x 1)
;   (display x))
; 
; (for (x 10)(> x 0)(- x 1)
;   (begin
;     (set! x (- x 1))
;     (for (y x)(< y 10)(+ y 1)
;       (begin (display " ")(display (+ y x))))))
; 
; (define let1 (let ((x (+ 5 5)))(+ x 1)))
; 
; (define let2 (let ((x (+ 1 2))
;                     (y (* 3 2)))
;                 (+ 1 x y)))
; 
; (define let3 (let* ((w 3)
;                     (x w)
;                     (y (+ x 2))
;                     (z (+ x y 5)))
;                (display z)
;                (newline)
;                (display x)
;                (* x z)))
; 
; (define (fib n)
;   (let fib-iter ((a 1) (b 0) (count n))
;     (if (= count 0)
;         b
;         (fib-iter (+ a b) 
;                   a 
;                   (- count 1)))))
; 
; (define g (let go ((xs '(1 2 3 4)))
;   (if (not (null? xs))
;     (begin (display (car xs))
;     (newline)
;     (go (cdr xs))))))



;4.24 w/ analysis


(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))        

(define (factorial x)(letrec
    ((fact
      (lambda (n)
        (if (= n 1)
            1
            (* n (fact (- n 1)))))))
  (fact x)))


(define (time-function fn)
  (let ((t1 (runtime)))
    (fn)
    (let ((t2 (runtime)))
      (cons "runtime: " (cons (- t2 t1) '())))))

;(time-function (lambda () (eval '(begin (define (factorial x)(letrec
;    ((fact
;      (lambda (n)
;        (if (= n 1)
;            1
;            (* n (fact (- n 1)))))))
;  (fact x)))(factorial 10000)) the-global-environment)))
;
;(time-function (lambda () (eval '(begin (define (fib n)
;  (let fib-iter ((a 1) (b 0) (count n))
;    (if (= count 0)
;        b
;        (fib-iter (+ a b) 
;                  a 
;                  (- count 1)))))(fib 100000)) the-global-environment)))
;
;(time-function (lambda () (eval '(begin (define (A x y)
;  (cond ((= y 0) 0)
;        ((= x 0) (* 2 y))
;        ((= y 1) 2)
;        (else (A (- x 1)
;                 (A x (- y 1)))))) (A 1 1)) the-global-environment)))

(define (unless pred con alt)(if pred alt con))

; 4.27
(define count 0)
(define (id x) (set! count (+ count 1)) x)
(define w (id (id 10)))
(define (zip f  as bs)
  (if (or (null? bs)(null? as)) '()
      (cons (f (car as) (car bs))'
            (zip f (cdr ls)))))
;count -> 0
;w -> 10 and count = 2
;4.29
(define (fib n)
  (let fib-iter ((a 1) (b 0) (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) 
                  a 
                  (- count 1)))))
