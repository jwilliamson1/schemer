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

;(arb-args'(+ x x x))

(define (arb-args-infix x)
  (if(null? (cdddr x)); only three arguments a + a
     (caddr x)
;else has more than that
     (cddr x)))
;(displayln "arb-args-infix")
;(arb-args-infix '(x + y + z))

;PREDICATES
(define (sum? x)
  (and (pair? x)(not(null? (cdr x))) (eq? (cadr x) '+)))
;(displayln "sum?")
;(sum? '(3 + x))

(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '^)))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
;(displayln "product?")
;(product? '(3 * 4))

(define (addend s) (car s))

;(null? (cdddr'(+ a b c)))

(define (augend s)(arb-args-infix s))
;(displayln "augend")
;(augend '(a + b + c))
;(augend '(a + b + c + d))

;check sum in
;we can add numbers directly or convert symbols into (* x 2)
(define (number-or-eq-symbol? exp1 exp2)
  (or(and (number? exp1)(number? exp2))
     (and (variable? exp1)(eq? exp1 exp2))))
;(displayln "number-or-eq-symbol?")
;(number-or-eq-symbol? 'x 'x)
;(number-or-eq-symbol? 'x 'y)
;(number-or-eq-symbol? 'x  1)
;(number-or-eq-symbol? '2  1)

(define (if-list e)
  (if(pair? e) e (list e)))

;(define (make-sum a1 a2)
;  (displayln (list "make-sum: " a1 a2))
;
;  (define (handle-vars v1 v2)
;    (displayln "handle-vars")
;    (if(and(variable? v1)(eq? v1 v2))
;       (list 2 '* v1)
;       (make-sum v1 v2)))
;
;  (define (flatten-one-side flat unknown)
;    (displayln (list "flatten-one-side" flat unknown))
;    (if(sum? unknown)
;       (append (list flat '+) unknown)
;       (list flat '+ unknown)))
;  
;  (define (simplify-num/symbol-sum n s)
;    (displayln (list "simplify-num-sum" n s))
;    (cond((number-or-eq-symbol?  n(addend s))(flatten-one-side(augend s)(make-sum n (addend s))))
;         (else (flatten-one-side(make-sum n (augend s))(addend s)))))
;  
;  (define (simplify-sums s1 s2)
;    (displayln "simplify-sums")
;    (cond((number-or-eq-symbol? (addend s1)(addend s2))(flatten-one-side(handle-vars (addend s1)(addend s2))(handle-vars (augend s1)(augend s2)))); first arg to flatten will either be a number or (x ^ 2)
;         ((number-or-eq-symbol?(augend s1)(addend s2))(flatten-one-side(handle-vars (augend s1)(addend s2))(handle-vars (addend s1)(augend s2))))
;         ((number-or-eq-symbol?(addend s1)(augend s2))(flatten-one-side(handle-vars (addend s1)(augend s2))(handle-vars (augend s1)(addend s2))))
;         ((number-or-eq-symbol? (augend s1)(augend s2))(flatten-one-side(handle-vars (addend s1)(augend s2))(handle-vars  (augend s1)(addend s2))))
;         (else(append a1 (list '+) a2))))
;  
;  (cond ((=number? a1 0) a2); take care of zeros
;        ((=number? a2 0) a1)
;        ((and (number? a1) (number? a2))(+ a1 a2)); if simple nums just add
;        ((or(product? a1)(exponentiation? a1))(displayln "a1 is prod or exp")(flatten-one-side a1 a2))
;        ((or(product? a2)(exponentiation? a2))(displayln "a2 is prod or exp")(flatten-one-side a2 a1))
;        ((and (sum? a1)(sum? a2))(displayln "two sums")(simplify-sums a1 a2))
;        ((and (sum? a1)(or(variable? a2)(number? a2)))(displayln "sum and num")(simplify-num/symbol-sum a2 a1))
;        ((and (sum? a2)(or(variable? a1)(number? a1)))(displayln "sum and num")(simplify-num/symbol-sum a1 a2))     
;        (else (displayln "else")(list a1 '+ a2))))

(define (make-operation oper oper? higher-oper)
  (lambda (a1 a2)
    (define (make-oper a1 a2)
      (displayln (list "make-oper: " a1 a2))
      
      (define (handle-vars v1 v2)
        (displayln "handle-vars")
        (if(and(variable? v1)(eq? v1 v2))
           (list 2 higher-oper v1)
           (make-oper v1 v2)))
      
      (define (flatten-to-right left right)
        (displayln (list "flatten-to-right" left right))
        (if(oper? right)
           (append (list left oper) right)
           (list left oper right)))

      (define (flatten-to-left left right)
        (displayln (list "flatten-to-left" left right))
        (if(oper? right)
           (append right (list oper left ))
           (list right oper (car left)(cadr left)(caddr left))))
      
      (define (simplify-num/symbol-oper n s)
        (displayln (list "simplify-num-oper" n s))
        (cond((number-or-eq-symbol?  n (addend s))(flatten-to-right(augend s)(handle-vars n (addend s))))
             (else (flatten-to-right(addend s)(make-oper n (augend s))))))
      
      (define (simplify-opers s1 s2)
        (displayln (list "simplify-opers" s1 s2))
        (cond((number-or-eq-symbol? (addend s1)(addend s2))(flatten-to-right(handle-vars (addend s1)(addend s2))(handle-vars (augend s1)(augend s2)))); first arg to flatten will either be a number or (x ^ 2)
             ((number-or-eq-symbol?(augend s1)(addend s2))(flatten-to-right(handle-vars (augend s1)(addend s2))(handle-vars (addend s1)(augend s2))))
             ((number-or-eq-symbol?(addend s1)(augend s2))(flatten-to-right(handle-vars (addend s1)(augend s2))(handle-vars (augend s1)(addend s2))))
             ((number-or-eq-symbol? (augend s1)(augend s2))(flatten-to-right(handle-vars (addend s1)(augend s2))(handle-vars  (augend s1)(addend s2))))
             (else
              (displayln "simplify-opers else")
              (append a1 (list oper) a2))))
      
      (cond ((=number? a1 0) a2); take care of zeros
            ((=number? a2 0) a1)
            ((and (number? a1) (number? a2))(+ a1 a2)); if simple nums just add
            ((and (or(product? a1)(exponentiation? a1))(or(product? a2)(exponentiation? a2)))
             (displayln "both exp/prod")
             (append a1 (list oper) a2))
            ((or(product? a1)(exponentiation? a1))
             (displayln "a1 is prod or exp")
             (flatten-to-right a1 a2))
            ((or(product? a2)(exponentiation? a2))
             (displayln "a2 is prod or exp")
             (flatten-to-left a2 a1))
            ((and (oper? a1)(oper? a2))
             (displayln "two sums")
             (simplify-opers a1 a2))
            ((and (oper? a1)(or(variable? a2)(number? a2)))
             (displayln "sum then single")
             (simplify-num/symbol-oper a2 a1))
            ((and (oper? a2)(or(variable? a1)(number? a1)))
             (displayln "single then sum")
             (simplify-num/symbol-oper a1 a2))     
            (else
             (displayln "else")
             (list a1 oper a2))))
    (make-oper a1 a2)))
(displayln "MAKE OPERATION TEST")
((make-operation '+ sum? '*) 5 6)

(define make-sum
  (make-operation '+ sum? '*))

(displayln "make-sum test")
(make-sum '(5 + x )'(x + 5))
(make-sum '(5 + x)'(2 + y))
(make-sum '(5 + x)'(2 + x))
(make-sum 'y '(y + 7))
(make-sum '(y + 7)'y)
(make-sum 7 '(x + 7))
(make-sum '(3 + x)'(6 + y))
(make-sum '(x + 5)'(6 + y))
(make-sum'(5 + y)'(7 ^ z))
(make-sum '(x + 6)(make-sum'(5 + y)'(7 ^ z))) ; should become x + 11 + y
(make-sum '(x + 6)'(5 ^ y)) ; should become x + 11 + y
(make-sum '(x + 6)'(5 * y)) ; should become (x + 6 + (5 * y))
(make-sum '(5 * y)'(x + 6)) ; should become (x + 6 + (5 * y))
(make-sum '(5 * y)'(x ^ 6)) ; should become ((5 * y)+(x ^ 6))
(make-sum '(x + y) (make-sum'(x + y)'(x + y)))
(make-sum '(3 * y)'(x + y))
(make-sum(make-sum  4 5)(make-sum 6 'x))
(make-sum(make-sum  4 5)(make-sum 'x 6))
(make-sum(make-sum 6 'x)(make-sum  4 5))
(make-sum(make-sum 'x 6)(make-sum  4 5))
(make-sum(make-sum 'x 6)(make-sum  4 'y))

;PRODUCT

(define (make-product m1 m2)
  
  (define (handle-vars v1 v2)
    (if(eq? v1 v2)
       (list v1 '^ 2)
       (make-product v1 v2)))
  
  (define (make-flat-sum num sum)
    (append (list num '*) (if-list sum)))
  
  (define (simplify-num-sum n s)
    (cond((number? (addend s))(make-flat-sum(make-product n (addend s))(list (augend s))))
         (else (make-flat-sum(make-product n (augend s))(list (addend s))))))
  
  (define (simplify-sums s1 s2)    
    (cond((number-or-eq-symbol?(addend s1)(addend s2))(make-flat-sum(handle-vars (addend s1)(addend s2))(handle-vars (augend s1)(augend s2))))
         ((number-or-eq-symbol?(augend s1)(addend s2))(make-flat-sum(handle-vars (augend s1)(addend s2))(handle-vars (addend s1)(augend s2))))
         ((number-or-eq-symbol?(addend s1)(augend s2))(make-flat-sum(handle-vars (addend s1)(augend s2))(handle-vars (augend s1)(addend s2))))
         ((number-or-eq-symbol?(augend s1)(augend s2))(make-flat-sum(handle-vars (addend s1)(augend s2))(handle-vars (augend s1)(addend s2))))
         (else(append m1 (list '+) m2))))

  (cond ((or (=number? m1 0) 
             (=number? m2 0)) 
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        ((and (number? m1) (number? m2))(+ m1 m2))
        ((or(sum? m1)(exponentiation? m1))(cons m1 (cons '* (cons m2 '()))))
        ((or(sum? m2)(exponentiation? m2))(cons m1 (cons '* (cons m2 '()))))
        ;((product? m1))
        ((and (product? m1)(product? m2))(simplify-sums m1 m2))
        ((and (product?  m1)(number? m2))(simplify-num-sum m2 m1))
        ((and (product?  m2)(number? m1))(simplify-num-sum m1 m2))   
        (else (append(if-list m1) (list '*)(if-list m2)))))

(displayln "book infix example");(x + 3 * (x + y + 2))
(make-sum 'x (make-product '3 (make-sum 'x (make-sum 'y 2))))

(displayln "make-product test")
(make-product(make-product  'x '4)(make-product 'x 7))
(make-product(make-product  'x '4)(make-product 'x 'y))
(make-product(make-product  'x 'y)(make-product 'x 5))
(make-product(make-product  'x 'y)(make-product 'x 'y))
(make-product(make-product  4 5)(make-product 6 'x))
(make-product(make-product  4 5)(make-product 6 7))
(make-product 'x (make-product 6 8))
(make-product '10 (make-product 6 8))
(make-product(make-product  'x 'y)(make-sum 'x 5))

(define (multiplier p) (car p))

(define (multiplicand p) (arb-args-infix p))
;(multiplicand '(3 * x))

;EXPONENTIATION
(define (make-exponentiation b e)
  (cond((and (=number? b 0)(=number? e 0))error "Zero to the zeroth power is undefined.")
       ((=number? e 0) 1)
       ((=number? e 1) b)
       ((and (number? b) (number? e)) 
        (expt b e))
       (else (list b '^ e))))



;(displayln "exponentiation")
;(exponentiation? '(^ x 4))

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



(deriv '(x +  3) 'x) ;1
(deriv '(x * y) 'x) ; y
(deriv '(x * 3 * y) 'x); 3 * y
(deriv '(x + (3 * (x + (y + 2)))) 'x)
(deriv '((y * x) * (x + 3)) 'x) ;((x * y) + (y * (x + 3))) OR y(x+3)+yx OR y(2x+3)
;;a x 2 + b x + c -> 2 a x + b 
(deriv '((a * (x ^ 2))+(b * x) + c) 'x)
(deriv '((4 * (x ^ 4))+(3 * (x ^ 3))+(2 * (x ^ 2))+(1 * (x ^ 1)) ) 'x)