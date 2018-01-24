#lang racket

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
  (cond((and (=number? b 0)(=number? e 0))error "Zero to the zeroth power is undefined.")
       ((=number? e 0) 1)
       ((=number? e 1) b)
       ((and (number? b) (number? e)) 
        (expt b e))
       (else (list '^ b e))))

(displayln "make-exponentation test")
(make-exponentiation 0 0); undefined
(make-exponentiation 0 1); 0
(make-exponentiation 1 0); 1
(make-exponentiation 1 1); 1
(make-exponentiation 2 1); 2
(make-exponentiation 2 2); 4

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(null? (cdddr'(+ a b c)))

(define (arb-args x)
  (if(null? (cdddr x))
     (caddr x)
     (cons '+ (cddr x))))

(define (augend s)(arb-args s))

(define (pure-pair? p)
  (cond((not(pair? p))#f)
       ((and(number? (cadr p))(number? (caddr p)))#t)
       (else #f)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        ((sum? a1)(displayln a1)
         (if(and(number? (addend a1))(pure-pair? a2))
                     (make-sum(make-sum a2 (addend a1)) (augend a1))
                     (make-sum(make-sum a2 (augend a1)) (addend a1))))
        ((sum? a2)(if(and(number? (addend a2))pure-pair? a1)
                     (make-sum(make-sum a1 (addend a2)) (augend a2))
                     (make-sum(make-sum a1 (augend a2)) (addend a2))))
        (else (list '+ a1 a2))))

(displayln "make-sum test")
(make-sum(make-sum  4 5)(make-sum 6 'x))
(make-sum(make-sum  4 5)(make-sum 'x 6))
(make-sum(make-sum 6 'x)(make-sum  4 5))
(make-sum(make-sum 'x 6)(make-sum  4 5))
(make-sum(make-sum 'x 6)(make-sum  4 'y))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) 
             (=number? m2 0)) 
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        (else (list '* m1 m2))))

(displayln "make-product test")
(make-product(make-product  4 5)(make-product 6 7))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '^)))

(displayln "exponentiation")
(exponentiation? '(^ x 4))

(define (base e) (cadr e))

(define (exponent e) (caddr e))




(displayln "augend")
(augend '(+ a b))
(augend '(+ a b c))



(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (arb-args p))

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
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation
                         (base exp)
                         (- (exponent exp) 1)))
          (deriv (base exp) var)))
        (else (error "unknown expression 
                      type: DERIV" exp))))

;(deriv '(+ x 3) 'x)
;(displayln "'(* x y) 'x)")
;(deriv '(* x 3 y) 'x)

;(deriv '(* (+ x y) (+ x 3)) 'x)
;a x 2 + b x + c
;(deriv '(+ (* a (^ x 2))(* b x) c) 'x)
;(deriv '(+ (* 6 (^ x 3))(* b x) c) 'x)
;(deriv '(+ (* 4(^ x 4))(* 3 (^ x 3))(* 2 (^ x 2))(* 1 (^ x 1)) ) 'x)