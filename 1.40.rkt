#lang racket

(define (inc x)
  (+ x 1))

(define (average x y)
  (/ (+ x y) 2))

(define (square x)(* x x))

(define (cube x) (* x x x))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


(define (average-damp f)
  (lambda (x) 
    (average x (f x))))


((average-damp square)5) ;15

;(define (sqrt x)
;  (fixed-point 
;   (average-damp 
;    (lambda (y) (/ x y)))
;   1.0))

(define (cube-root x)
  (fixed-point
   (average-damp
    (lambda (y)(/ x y)))
   1.0));<-1st guess

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) 
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) 
               guess))

(define (sqrt x)
  (newtons-method 
   (lambda (y) 
     (- (square y) x)) 
   1.0))

(define (fixed-point-of-transform 
         g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-ad x)
  (fixed-point-of-transform 
   (lambda (y) (/ x y))
   average-damp
   1.0))

(display "avg damp: ")
(sqrt-ad 19083)

(define (sqrt-nt x)
  (fixed-point-of-transform 
   (lambda (y) (- (square y) x))
   newton-transform
   1.0))
(display "newton: ")
(sqrt-nt 19083)

(define (cubic a b c)
  (lambda(x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))
(newtons-method(cubic 3 3 3) 1)

(define zero -2.259921049894855)
;check
((cubic 3 3 3) zero)

(define (double f)
  (lambda(x)
    (f (f x))))

((double inc)1) ;2

;(double *double
;  (lamba(x)
;        (*double (*double x))))
;
;(double  (lamba(x)(*double (*double x)))
;(double  (lamba(inc)(*double (*double inc)))
;
;(double2 (lambda (x)(f (f x)))
(((double (double double)) inc) 5)