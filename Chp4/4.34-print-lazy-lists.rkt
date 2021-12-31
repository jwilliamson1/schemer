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

(define *the-table* (make-hash))
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
  ;(display (list '***apply procedure))
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

(define (or? exp)
  (tagged-list? exp 'or))

(define (or->if preds)
  (if (null? (cdr preds))
      'false
      (make-if (cadr preds) 'true
               (or->if (cdr preds)))))

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
(define (until->while exp)
  (let ((check (cadr exp))
        (body (caddr exp)))
    (while->lambda (list 'while (list 'not check)
          body))))
(define (make-let var-defs body)
  (cons 'let (cons (list var-defs) body)))

(define (make-procedure parameters body env)  
    (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
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

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'mcons cons)
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
        (list 'scons cons)
        (list 'scar car)
        (list 'scdr cdr)
        (list 'list list)
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

(define (prompt-for-input string)
  (newline) (newline) 
  (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define the-global-environment 
  (setup-environment))


(define (lazy-pair? exp)
  (tagged-list? exp 'lazy-list))

(define (display-list lazy-pair-syntax isFirst) ; lazy-list (lambda (m)... 
  (let ((lambda-representation (cdr lazy-pair-syntax))) ; (lambda (m) ...
    (let ((x (force-it (lookup-variable-value 'x (procedure-environment lambda-representation))))
          (y (force-it (lookup-variable-value 'y (procedure-environment lambda-representation)))))
      (if isFirst (display "("))
      (if (lazy-pair? x)
          (display-list x true)
          (display  x))
      (cond ((lazy-pair? y)(begin (display " ")(display-list y false)))
            ((eq? y '()) (display ")"))
            (else                
             (begin (display " . ")
                    (user-print y)
                    (display ")")))))))
   ; (user-print (cdr lst)))) ;bind cdr to same m

(define (user-print object)
  (if (lazy-pair? object)
      (display-list object true)
      (if (compound-procedure? object)
          (display 
           (list 'compound-procedure
                 (procedure-parameters object)
                 (procedure-body object)
                 '<procedure-env>))
          (display object))))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(eval '(define (cons x y) (scons 'lazy-list (lambda (m) (m x y)))) the-global-environment)
(eval '(define (car z) ((scdr z) (lambda (p q) p))) the-global-environment)
(eval '(define (cdr z) ((scdr z) (lambda (p q) q))) the-global-environment)
(eval '(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1)))) the-global-environment)

(eval '(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items))))) the-global-environment)

(eval '(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items)) the-global-environment)

(eval '(define (add-lists list1 list2)
  (cond ((null? list1) list2)
        ((null? list2) list1)
        (else (cons (+ (car list1) 
                       (car list2))
                    (add-lists
                     (cdr list1) 
                     (cdr list2)))))) the-global-environment)

(eval '(define ones (cons 1 ones)) the-global-environment)
(eval '(define integers 
  (cons 1 (add-lists ones integers))) the-global-environment)

(eval '(define (integral integrand initial-value dt)
  (define int
    (cons initial-value
          (add-lists (scale-list integrand dt) 
                     int)))
  int) the-global-environment)

(eval '(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (map f y))
  y) the-global-environment)
; good that this works
;(actual-value '(car (cdr (cons 1 (cons 2 '())))) the-global-environment)
;(actual-value '(cons 1 2) the-global-environment)
(driver-loop)
