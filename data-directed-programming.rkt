#lang racket
 (define *the-table* (make-hash));make THE table 
 (define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value));put 
 (define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f));get

(define (square x)(* x x))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: 
              TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: 
              CONTENTS" datum)))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) 
    (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) 
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

(install-rectangular-package)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

(install-polar-package)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types: 
             APPLY-GENERIC"
            (list op type-tags))))))

(define (real-part z) 
  (apply-generic 'real-part z))
(define (imag-part z) 
  (apply-generic 'imag-part z))
(define (magnitude z) 
  (apply-generic 'magnitude z))
(define (angle z) 
  (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 
        'rectangular) 
   x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 
        'polar) 
   r a))

;test complex number package
(define rectangular1 (make-from-real-imag 5 3))
rectangular1
(define samePolar (make-from-mag-ang (magnitude rectangular1)(angle rectangular1)))
samePolar
(define backToRect (make-from-real-imag (real-part samePolar)(imag-part samePolar)))
backToRect

;cannot add same-variable and number because they don't have operators
(define (install-deriv-package)
  
  (define (=number? exp num)
    (and (number? exp) (= exp num)))
    
  (define (variable? x) (symbol? x))
  
  (define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))
  
  (define (addend s) (cadr s))
  
  (define (multiplier p) (cadr p))
  
  (define (make-sum-list l) 
    (if (= (length l) 2) 
        (list '+ (car l) (cadr l)) 
        (make-sum (car l) (make-sum-list (cdr l)))))
  
 (define (make-sum a1 a2) 
   (cond ((=number? a1 0) a2) 
         ((=number? a2 0) a1) 
         ((and (number? a1) (number? a2)) (+ a1 a2)) 
         (else (make-sum-list (list a1 a2))))) 
  
 (define (make-product-list l) 
   (if (= (length l) 2) 
       (list '* (car l) (cadr l)) 
       (make-product (car l) (make-product-list (cdr l)))))
  
 (define (make-product m1 m2) 
   (cond ((or (=number? m1 0) (=number? m2 0)) 0) 
         ((=number? m1 1) m2) 
         ((=number? m2 1) m1) 
         ((and (number? m1) (number? m2)) (* m1 m2)) 
         (else (make-product-list (list m1 m2))))) 
  
 (define (augend s) 
   (let ((a (cddr s))) 
     (if (= (length a) 1) 
         (car a) 
         (make-sum-list a))))

 (define (multiplicand p) 
   (let ((m (cddr p)))
     (if (= (length m) 1) 
         (car m) 
         (make-product-list m))))

  
  (define (operator exp) (car exp))
  (define (operands exp) (cdr exp))
  
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  
  (define (deriv-product exp var)
    (make-sum
           (make-product 
            (multiplier exp)
            (deriv (multiplicand exp) var))
           (make-product 
            (deriv (multiplier exp) var)
            (multiplicand exp))))

  (define (base e) (cadr e))

  (define (exponent e) (caddr e))

  (define (make-exponentiation b e)
  (cond((and (=number? b 0)(=number? e 0))error "Zero to the zeroth power is undefined.")
       ((=number? e 0) 1)
       ((=number? e 1) b)
       ((and (number? b) (number? e)) 
        (expt b e))
       (else (list '^ b e))))

  (define (deriv-exponent exp var)
             (make-product
          (make-product (exponent exp)
                        (make-exponentiation
                         (base exp)
                         (- (exponent exp) 1)))
          (deriv (base exp) var)))

    (define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp) 
           (if (same-variable? exp var) 
               1 
               0))
          (else ((get 'deriv (operator exp)) 
                 exp
                 var))))
  
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '^ deriv-exponent)
  (put 'deriv 'deriv deriv)
  'done)

(install-deriv-package)

(define (deriv exp var)
  ((get 'deriv 'deriv)
   exp var))

(deriv '(+ x 3) 'x) ;1
(deriv '(* x y) 'x) ; y
(deriv '(* x 3 y) 'x); 3 * y
(deriv '(* (* y x)(+ x 3)) 'x)
(deriv '(+ (* a (^ x 2))(* b x) c) 'x)
(deriv '(^ 10 (^ x 2)) 'x)