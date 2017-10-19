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

(define double
  (λ(f)(λ(x)(f (f x)))))

(((double (double double)) inc) 5);21

(define compose
  (λ (f g)
    (λ (x)
      (f (g x)))))

(define repeated1
  (λ (f n)
    (λ (x)
      (define repeat
        (λ(a)(+ a a)))
      (repeat x))))


((compose square(compose square (λ(x)x)))5)

(((λ (f)
  (compose f(compose f (λ(x)x))))square)5)

(((λ (f n)     
    (if(= n 0)
       (compose f (λ(x)x))
       "ELSE"))
  square 0)
 5)

(define (sub1 n)
  (- n 1))

(sub1 234)
;works

(define (repeated f n)
  (cond((= n 0)(λ(x)x))
       ((even? n)(repeated(compose f f)(/ n 2)))
       (else(compose f (repeated f (- n 1))))))

;(repeated square 1)5)
;((repeated square 2)5)

(define (smooth f)
  (lambda (x)
    (/ (+ (f x)
          (f (+ x dx))
          (f (- x dx)))
       3)))

 (define (n-fold-smooth f n)  
   ((repeated smooth n) f))  

(((repeated smooth 16)square)2)












;(((double (λ(x)(double (double x)))) inc) 5);21

;(((λ(x)
;    ((λ(x)(double(double x)))
;     ((λ(x)(double(double x))) x)))
;  inc)
; 5);21
;
;(((λ(x0)
;    ((λ(x2)(double(double x2)))
;     ((λ(x1)(double(double x1))) x0)))
;  inc)
; 5);21
;
;(((λ(x2)(double(double x2)))
;  ((λ(x1)(double(double x1))) inc))
; 5);21
;
;(((λ(x2)(double(double x2)))(double(double inc))) 5);21
;
;(((λ(x2)(double(double x2)))(double(λ (x)(inc(inc x))))) 5);21
;
;(((λ(x2)(double(double x2)))
;  (λ (x)((λ (x)(inc(inc x)))
;         ((λ (x)(inc(inc x)))
;          x)))) 5);21
;
;((λ (x)((λ (x)(inc(inc x)))
;        ((λ (x)(inc(inc x)))
;         x)))5)
;
;(λ (x)((λ (x)(inc(inc x)))
;        ((λ (x)(inc(inc x)))
;         x)))
;;is equivalent to
;((λ (x) (inc(inc(inc(inc x)))))5)
;
;((double(double (λ (x)((λ (x)(inc(inc x)))
;                       ((λ (x)(inc(inc x)))
;                        x)))))
; 5);21
;
;((double(λ (x)((λ (x)((λ (x)(inc(inc x)))
;                      ((λ (x)(inc(inc x)))
;                       x)))((λ (x)((λ (x)(inc(inc x)))
;                                   ((λ (x)(inc(inc x)))
;                                    x))) x))))
; 5);21
;
;((double(λ (x)((λ (x)((λ (x)(inc(inc x)))
;                      ((λ (x)(inc(inc x)))
;                       x)))
;               ((λ (x)((λ (x)(inc(inc x)))
;                                   ((λ (x)(inc(inc x)))
;                                    x))) x))))
; 5);21
;
;((λ (x)((λ (x)((λ (x)((λ (x)(inc(inc x)))
;                           ((λ (x)(inc(inc x)))
;                            x)))
;                    ((λ (x)((λ (x)(inc(inc x)))
;                            ((λ (x)(inc(inc x)))
;                             x))) x))) ((λ (x)((λ (x)((λ (x)(inc(inc x)))
;                                                      ((λ (x)(inc(inc x)))
;                                                       x)))
;                                               ((λ (x)((λ (x)(inc(inc x)))
;                                                       ((λ (x)(inc(inc x)))
;                                                        x))) x))) x)))5)
;
;
;
;((double inc)5) ;7
;
;(((double double)inc)5);9
;
;(((double (double double)) inc) 5);21
;
;	(((lambda(f2)(lambda(x2)(f2 (f2 x2)))) 
;		((lambda(f2)(lambda(x2)(f2 (f2 x2)))) inc))5) 	
; (display "should be 9: ")((lambda(x2)((lambda(x2)(inc (inc x2))) ((lambda(x2)(inc (inc x2))) x2)))5)
;((lambda(x2)(inc (inc x2))) ((lambda(x2)(inc (inc x2))) 5))
;;omg this sucks but it works
;((lambda(x2)(inc (inc x2))) (inc (inc 5)))
;
;(((lambda(x1)
;	((lambda(f2)(lambda(x2)(f2 (f2 x2)))) 
;		((lambda(f2)(lambda(x2)(f2 (f2 x2)))) x1)))  
;
;		((lambda(f2)(lambda(x2)(f2 (f2 x2)))) 
;			((lambda(f2)(lambda(x2)(f2 (f2 x2)))) inc)))5)	
;
;(((lambda(x1)
;	((lambda(f2)(lambda(x2)(f2 (f2 x2)))) 
;		((lambda(f2)(lambda(x2)(f2 (f2 x2)))) x1)))  
;
;		((lambda(f2)(lambda(x2)(f2 (f2 x2)))) 
;			(lambda(x2)( inc ( inc x2)))))5)
;
;(((lambda(f2)(lambda(x2)(f2 (f2 x2)))) 
;  (lambda(x2)((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;                          ((lambda(x2)( inc ( inc x2))) x2)))
;              ((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;                           ((lambda(x2)( inc ( inc x2))) x2))) x2))))5)					
;((lambda(x2)
;				((lambda(x2)((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) x2))) ((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) x2))) x2))) 
;			
;				((lambda(x2)((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) x2))) ((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) x2))) x2)))  x2)))	
;	  	
;5)
;
;((lambda(x2)((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) x2))) ((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) x2))) x2))) 
;			
;				((lambda(x2)((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) x2))) ((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) x2))) x2)))  5))
;
;((lambda(x2)((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) x2))) ((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) x2))) x2))) 
;			
;				((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) x2))) ((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) x2))) 5)))
;
;((lambda(x2)((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) x2))) 
;					((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) x2))) x2))) 
;			
;				((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) x2))) 
;					((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) 5))))
;((lambda(x2)((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) x2))) 
;					((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) x2))) x2))) 
;			
;				((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) x2))) 
;					((lambda(x2)( inc ( inc x2))) 
;					( inc ( inc 5)))))
;
;((lambda(x2)((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) x2))) 
;					((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) x2))) x2))) 
;			
;				((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) x2))) 
;					((lambda(x2)( inc ( inc x2))) 
;					7)))
;((lambda(x2)((lambda(x2)((lambda(x2)( inc ( inc x2)))  
;					((lambda(x2)( inc ( inc x2))) x2))) 
;					((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) x2))) x2))) 
;			
;				((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) x2))) 
;					( inc ( inc 7))))
;((lambda(x2)((lambda(x2)((lambda(x2)( inc ( inc x2)))  
;					((lambda(x2)( inc ( inc x2))) x2))) 
;					((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) x2))) x2))) 
;			
;				((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) x2))) 
;					9))
;((lambda(x2)((lambda(x2)((lambda(x2)( inc ( inc x2)))  
;					((lambda(x2)( inc ( inc x2))) x2))) 
;					((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) x2))) x2))) 
;			
;				((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) 9)))
;
;((lambda(x2)((lambda(x2)((lambda(x2)( inc ( inc x2)))  
;					((lambda(x2)( inc ( inc x2))) x2))) 
;					((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) x2))) x2))) 
;			
;				13)
;((lambda(x2)((lambda(x2)( inc ( inc x2)))  
;					((lambda(x2)( inc ( inc x2))) x2))) 
;					((lambda(x2)((lambda(x2)( inc ( inc x2))) 
;					((lambda(x2)( inc ( inc x2))) x2))) 13))
;
;((lambda(x2)((lambda(x2)( inc ( inc x2)))  
;					((lambda(x2)( inc ( inc x2))) x2))) 
;					17)	