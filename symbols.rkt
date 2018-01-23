

(cons 'a 'b)
(cons 'a 2)
(define b 3)

(cons 'a b)

(car '(a b c))

(cadddr (quote(a b c 3)))

(list 'a 'b 'c 'abc)
;'(a b c)
(list (list 'george))
;'((george))
(cdr '((x1 x2) (y1 y2)))
;'((y1 y2))
(cadr '((x1 x2) (y1 y2)))
;'(y1 y2)
(pair? (car '(a short list)))
;false
(memq 'red '((red shoes) (blue socks)))
;false
(memq 'red '(red shoes blue socks))
;true

;(define (equal? a b)
;  (or
;   (eq? a b)
;   (and
;    (pair? a)
;    (pair? b)
;    (equal? (car a)(car b))
;    (equal? (cdr a)(cdr b)))))

(car '(a b c))
(car (quote (a b c)))
(car '(list 'quote a b c))

(car '(quote a))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (make-exponentiation b e)
  (cond((and (=number? b 0)(=number? e 0))1)
       ((=number? e 0) 1)
       ((=number? e 1) b)
       ((and (number? a1) (number? a2)) 
        (expt a1 a2))
       (else (list '^ b e))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) 
             (=number? m2 0)) 
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        (else (list '* m1 m2))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '^)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        (else (error "unknown expression 
                      type: DERIV" exp))))

(deriv '(+ x 3) 'x)

(deriv '(* x y) 'x)

(deriv '(* (* x y) (+ x 3)) 'x)