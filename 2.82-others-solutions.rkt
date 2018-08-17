#lang sicp
;(define *the-table* (make-hash));make THE table
;(define *coercion-table* (make-hash));make THE table 
;(define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value));put 
;(define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f));get
;(define (put-coercion key1 key2 value) (hash-set! *coercion-table* (list key1 key2) value))
;(define (get-coercion key1 key2) (hash-ref *coercion-table* (list key1 key2) #f));get

(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (put-coercion op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

(define (get-coercion op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

(define (attach-tag type-tag contents)
  (if (integer? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((integer? datum) 'scheme-number)           
        ((pair? datum)(car datum))
        (else
      (error "Bad tagged datum: 
              TYPE-TAG" datum))))

(define (contents datum)
  (cond ((integer? datum) datum)   
        ((pair? datum)(cdr datum))
        (else(error "Bad tagged datum: 
              CONTENTS" datum))))

 (define (filter proc seq) 
   (cond ((null? seq) nil) 
         ((proc (car seq)) 
          (cons (car seq) (filter proc (cdr seq)))) 
         (else 
          (filter proc (cdr seq)))))

(define (andmap f xs)
    (cond ((null? xs) #t)
          ((f (car xs))
            (andmap f (cdr xs)))
          (else #f)))

(define (ormap f xs)
    (cond ((null? xs) #f)
          ((f (car xs)) #t)
          (else (ormap f (cdr xs)))))
  
(define (apply-generic op . args)
  (define (can-coerce-into? types target-type)
    "Can all _types_ be coerced into _target-type_ ?" 
    (andmap 
      (lambda (type)
        (or
          (equal? type target-type)
          (get-coercion type target-type)))
      types))
  (define (find-coercion-target types)
    "Find a type among _types_ that all _types_ can be
    coerced into." 
    (ormap
      (lambda (target-type)
        (if (can-coerce-into? types target-type)
          target-type
          #f))
      types))
  (define (coerce-all args target-type)
    "Coerce all _args_ to _target-type_" 
    (map 
      (lambda (arg)
        (let ((arg-type (type-tag arg)))
          (if (equal? arg-type target-type)
            arg
            ((get-coercion arg-type target-type) arg))))
      args))
  (define (no-method type-tags)
    (error "No method for these types" 
      (list op type-tags)))      
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (let ((target-type (find-coercion-target type-tags)))
          (if target-type
            (apply
              apply-generic
              (append 
                (list op)
                (coerce-all args target-type)))
            (no-method type-tags)))))))  

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))


;; scheme numbers
(define (install-scheme-number-package)
  (define (scheme->rational n)
    (make-rational n 1))
  (define (exp x y) 
    (apply-generic 'exp x y))
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y)  (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'exp 
       '(scheme-number scheme-number)
       (lambda (x y) 
         (tag (expt x y))))
  (put 'raise
       '(scheme-number)
       (lambda (x) (scheme->rational x)))  
  'done)

(install-scheme-number-package)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))



(apply-generic 'equ? (make-scheme-number 3)(make-scheme-number 3))
(apply-generic 'equ? (make-scheme-number 5)(make-scheme-number 5))
(display "=zero? should be false: " )
(apply-generic '=zero? (make-scheme-number 4))
(display "=zero? should be true: " )
(apply-generic '=zero? (make-scheme-number 0))

;; rational package
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ?-rat r1 r2)
    (and (= (numer r1)(numer r2))
         (= (denom r1)(denom r2))))
  (define (=zero?-rat r)
    (= (numer r) 0))
  (define (rational->real r)
    (make-real(/ (numer r)(denom r))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (x y) (equ?-rat x y)))
  (put '=zero? '(rational)
       (lambda (x) (=zero?-rat x)))
  (put 'raise '(rational)
       (lambda (x) (rational->real x)))
  (put 'project
       '(rational)
       (lambda (x) (make-scheme-number (numer x))))
  'done)

(install-rational-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define r1 (make-rational 3 4))
(define r2 (make-rational 75 100))
(define r3 (make-rational 2 3))

;(displayln "should be true")
(apply-generic 'equ? r1 r2)
;(displayln "should be false")
(apply-generic 'equ? r1 r3)

(define r4 (make-rational 0 5))
;(display "=zero? r4 should be true: ")
(apply-generic '=zero? r4)
;(display "=zero? r3 should be false: ")
(apply-generic '=zero? r3)

(define (install-real-package)
  ;; internal procedures
  (define (exp x y) 
    (apply-generic 'exp x y))  
  (define (real->complex r)
    (make-complex-from-real-imag r 0))
  ;; interface to the rest of the system
  (define (tag x)(attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real
       (lambda (x) (tag (* 1.0 x))))
  (put 'equ? '(real real)
       (lambda (x y)  (= x y)))
  (put '=zero? '(real)
       (lambda (x) (= x 0)))
  (put 'exp 
       '(real real)
       (lambda (x y) 
         (tag (expt x y))))
  (put 'raise
       '(real)
       (lambda (r) (real->complex r)))
  (put 'project
       '(real)
       (lambda (r) (make-rational (round r) 1)))
  'done)

(install-real-package)

(define (make-real n)
  ((get 'make 'real) n))

(define re1 (make-real 2.35))
(define re2 (make-real 3/4))
(define re3 (make-real .75))
(define rt1 (make-rational 8 3))

((get 'make 'real) 3.572546)

re1
re2
re3
rt1
(apply-generic 'equ? re1 re2)
(apply-generic 'equ? re2 re3)

(define (install-rectangular-package)
  ;; internal procedures
  (define (square x)(* x x))
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
  (define (equ? p1 p2)
    ((and (= (real-part p1)(real-part p2))
          (= (imag-part p1)(imag-part p2)))))
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
  (put 'equ? '(rectangular) equ?)
  'done)

(install-rectangular-package)

(define (install-polar-package)
  ;; internal procedures
  (define (square x)(* x x))
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
  (define (equ? p1 p2)
    (and (= (magnitude p1)(magnitude p2))
          (= (angle p1)(angle p2))))
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
  (put 'equ? '(polar polar) equ?)
  'done)

(install-polar-package)

(define (make-from-mag-ang r a)
  ((get 'make-from-real-imag 'polar) r a))

(define (equ?-polar r a)
  ((get 'equ? '(polar polar)) r a))

(define p1 (make-from-mag-ang 5 3))
(define p2 (make-from-mag-ang 5 3))
(define p3 (make-from-mag-ang 5 4))

;(displayln "Should be true")
(apply-generic 'equ? p1 p2)
;(displayln "Should be false")
(apply-generic 'equ? p1 p3)

(define (real-part c)
  (apply-generic 'real-part c))

(define (imag-part c)
  (apply-generic 'imag-part c))

(define (magnitude c)
  (apply-generic 'magnitude c))

(define (angle c)
  (apply-generic 'angle c))


(define (install-complex-package)
  ;; imported procedures from rectangular 
  ;; and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 
          'rectangular) 
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) 
     r a))
  ;; internal procedures
  (define (=zero?-complex z)
    (= (real-part z)(imag-part z)))
  (define (equ?-complex z1 z2)
    (and (= (real-part z1)(real-part z2))
         (= (imag-part z1)(imag-part z2))))
  (define (add-complex z1 z2)
    (make-from-real-imag 
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag 
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang 
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang 
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put '=zero? '(complex)
       (lambda (z)
         (=zero?-complex z)))
  (put 'equ? '(complex complex)
       (lambda (z1 z2)
         (equ?-complex z1 z2)))
  (put 'add '(complex complex)
       (lambda (z1 z2)
         ;re-tag as complex
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) 
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) 
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) 
         (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'project
       '(complex)
       (lambda (c)(make-real(* 1.0 (real-part c)))))
  'done)


  
 ;; The following code may be used to try out the solution and see it working. Also you will  
 ;;;need the scheme-number, rational and complex packages and associated generic declarations  
 ;;;;which are not given here. 
  
  
; (define *coercion-table* (make-equal-hash-table)) 
  
; (define (put-coercion type1 type2 proc) 
;   (hash-table/put! *coercion-table* (list type1 type2) proc)) 
  
; (define (get-coercion type1 type2) 
;   (hash-table/get *coercion-table* (list type1 type2) '())) 

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


 (define (install-coercion-package)  
 (define (scheme-number->complex n) 
   (make-complex-from-real-imag (contents n) 0)) 
 (define (scheme-number->rational n) 
   (make-rational (contents n) 1)) 
 (put 'scheme-number 'rational scheme-number->rational) 
 (put 'scheme-number 'complex scheme-number->complex) 
 'done) 
  
 (install-coercion-package) 
  
 ;;The following are some example evaluations 
  
 ;;RESULT 
 ;; 1 ]=>
(apply-generic 'add (make-scheme-number 1) (make-scheme-number 4)) 
  
 ;; ;Value: 5 
  
 ;; 1 ]=> (add (make-complex-from-real-imag 1 1) (make-complex-from-real-imag 3 2)) 
  
 ;; ;Value 14: (complex rectangular 4 . 3) 
  
 ;; 1 ]=> (add (make-scheme-number 1) (make-complex-from-real-imag 1 1)) 
  
 ;; ;Value 15: (complex rectangular 2 . 1) 
  
 ;; 1 ]=> (add (make-scheme-number 2) (make-rational 3 4)) 
  
 ;; ;Value 16: (rational 11 . 4) 
  
 ;; 1 ]=>  