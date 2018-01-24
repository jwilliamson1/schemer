#lang racket

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))



(define (arb-args x)
  (if(null? (cdddr x))
     (caddr x)
     (cons '+ (cons(cddr x)null))))

(arb-args'(+ x x x))

(define (arb-args-infix x)
  (if(null? (cdddr x)); only three arguments a + a
     (caddr x)
;else has more than that
     (cddr x)))
(displayln "arb-args-infix")
(arb-args-infix '(x + y + z))

;PREDICATES
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
(displayln "sum?")
(sum? '(3 + x))

(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '^)))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
(displayln "product?")
(product? '(3 * 4))

(define (addend s) (car s))

(null? (cdddr'(+ a b c)))

(define (augend s)(arb-args-infix s))
(displayln "augend")
(augend '(a + b + c))
(augend '(a + b + c + d))

;check sum in


(define (make-sum a1 a2)
  (define (simplify-sums s1 s2)    
    
    (if(sum? s2)
       (append s1 (cdr s2))
       (append s1 (list s2))))
  (cond ((=number? a1 0) a2); take care of zeros
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))(+ a1 a2))
        ((and (or(product? a1)(exponentiation? a1))(or(product? a2)(exponentiation? a2)))(append a1 (list '+) a2))
        ((or(product? a1)(exponentiation? a1))(append (list a1) (list '+)  a2))
        ((or(product? a2)(exponentiation? a2))(append a1 (list '+) (list a2)))
        ((and (sum? a1)(sum? a2))(append a1 (list '+) a2))
         ;if just numbers just add em
         ;at this point one of the arguments is not a simple number
         ;could be a sum, product, or exponentiation like (6 (* 5 x)) or (+ (^ x 5) 7)
        ;but it could be (+ x y) 3)
        ;((sum? a1)(simplify-sums a1 a2))
        ;((sum? a2)(append (list '+) (list a1) (cdr a2)))
        
        (else (list a1 '+ a2))))

(displayln "make-sum test")
(make-sum '(x + 6)(make-sum'(5 + y)'(7 ^ z))) ; should become x + 11 + y
(make-sum '(x + 6)'(5 ^ y)) ; should become x + 11 + y
(make-sum '(x + 6)'(5 * y)) ; should become (x + 6 + (5 * y))
(make-sum '(5 * y)'(x + 6)) ; should become (x + 6 + (5 * y))
(make-sum '(5 * y)'(x ^ 6)) ; should become ((5 * y)+(x ^ 6))
;(make-sum '(+ x y) (make-sum'(+ x y)'(+ x y)))
;(make-sum '(+ x 5 5)'(* 3 y))
;(make-sum '(+ x y y)'(* 3 y))
;(make-sum '(* 3 y)'(+ x y))
(make-sum(make-sum  4 5)(make-sum 6 'x))
(make-sum(make-sum  4 5)(make-sum 'x 6))
(make-sum(make-sum 6 'x)(make-sum  4 5))
(make-sum(make-sum 'x 6)(make-sum  4 5))
(make-sum(make-sum 'x 6)(make-sum  4 'y))



;PRODUCT

(define (make-product m1 m2)

  (cond ((or (=number? m1 0) 
             (=number? m2 0)) 
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        ;((product? m1))
        (else (list m1 '* m2))))


(displayln "make-product test")
(make-product(make-product  'x '4)(make-product 'x 7))
(make-product(make-product  4 5)(make-product 6 7))



(define (multiplier p) (car p))

(define (multiplicand p) (arb-args-infix p))
(multiplicand '(3 * x))

;EXPONENTIATION
(define (make-exponentiation b e)
  (cond((and (=number? b 0)(=number? e 0))error "Zero to the zeroth power is undefined.")
       ((=number? e 0) 1)
       ((=number? e 1) b)
       ((and (number? b) (number? e)) 
        (expt b e))
       (else (list '^ b e))))



(displayln "exponentiation")
(exponentiation? '(^ x 4))

(define (base e) (car e))

(define (exponent e) (caddr e))
;

;
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

(deriv '(x +  3) 'x)
(displayln "'(* x y) 'x)")
(deriv '(x * 3 * y) 'x)
;
(deriv '((x + y) * (x + 3)) 'x)
;;a x 2 + b x + c -> 2 a x + b 
(deriv '((a * (x ^ 2))+(b * x) + c) 'x)
(deriv '((4 * (x ^ 4))+(3 * (x ^ 3))+(2 * (x ^ 2))+(1 * (x ^ 1)) ) 'x)