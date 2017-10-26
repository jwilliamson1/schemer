#lang racket
(define logB 
    (lambda (x B) 
      (/ (log x) (log B))))

(define identity
  (lambda (x) x))

(define (inc x)
  (+ x 1))

(define (average x y)
  (/ (+ x y) 2))

(define (square x)(* x x))

(define (cube x) (* x x x))

(define tolerance 0.00001)

;original method
;(define (fixed-point f first-guess)
;  (define (close-enough? v1 v2)
;    (< (abs (- v1 v2)) 
;       tolerance))
;  (define (try guess)
;    (let ((next (f guess)))
;      (if (close-enough? guess next)
;          next
;          (try next))))
;  (try first-guess))

(define (fixed-point improve first-guess)  
    (define (close-enough? v1 v2)
      (< (abs (- v1 v2)) 
         tolerance))
    ((iterative-improvement close-enough? improve)first-guess))

(define (iterative-improvement good-enough? improve)
  (λ (first-guess)
    (define (try guess)
      (let ((next (improve guess)))
        (if (good-enough? guess next)
            next
            (try next))))
    (try first-guess)))

(define (sqrt x)
  (define (improve guess)
    (λ (guess)
      (average guess (/ x guess))))
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  ((iterative-improvement good-enough? improve) 1.0))

(display "should be 10 ")(sqrt 100)
    

(define (average-damp f)
  (lambda (x) 
    (average x (f x))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) 
               guess))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) 
            ((deriv g) x)))))


((average-damp square)5) ;15

;(define (sqrt x)
;  (fixed-point 
;   (average-damp 
;    (lambda (y) (/ x y)))
;   1.0))

(define (cube-root-old x)
  (fixed-point
   (average-damp
    (lambda (y)(/ x (square y))))
   1.0));<-1st guess

(display "Cube root of 27 is 3 ")(cube-root-old 27)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(display "75.00014999664018")((deriv cube) 5)





;(define (sqrt x)
;  (newtons-method 
;   (lambda (y) 
;     (- (square y) x)) 
;   1.0))

(define (fixed-point-of-transform g transform guess)
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

(define (repeated f n)
  (cond((= n 0)(λ(x)x))
       ((even? n)(repeated(compose f f)(/ n 2)))
       (else(compose f (repeated f (- n 1))))))

(define (smooth f)
  (lambda (x)
    (/ (+ (f x)
          (f (+ x dx))
          (f (- x dx)))
       3)))

 (define (n-fold-smooth f n)  
   ((repeated smooth n) f))  

(define smooth-sq ((repeated smooth 3)square))

(define (cube-root x)
  (fixed-point-of-transform
   (lambda(y)(/ x (square y)))
   average-damp
   1.0))

(display "Cube root of 27 is 3 ")(cube-root 27)


(define (nth-root n x)
  (fixed-point-of-transform
   (λ (y)(/ x (expt y (- n 1))))
   (repeated average-damp (floor(logB n 2)))
   1.0))

(nth-root 6 64)


    
  
  

;(display "thrice: ")(smooth-sq 2)
;
;(display "twice: ")((smooth (smooth square))2)
;
;
;(square 2)
;(square (+ 2 dx))
;(square (- 2 dx))
;;demonstrate smooth being applied to an already smooth square 
;(/ (+(identity 4)
;     (identity (+ 4.0000400001 dx))
;     (identity(- 3.9999600000999997 dx)))
;   3)
;
;(display "twice: ")(/ (+ ((smooth(smooth square)) 2)
;                         ((smooth(smooth square)) (+ 2 dx))
;                         ((smooth(smooth square)) (- 2 dx)))
;                      3)
;      
;
;((smooth square)2)
;((smooth square)(+ 2 dx))
;((smooth square)(- 2 dx))
;;smooth again
;(/ 
;(+((smooth identity)4.000000000066667)
;((smooth identity)(+ 4.000040000166667 dx))
;((smooth identity)(- 3.9999600001666664 dx)))3)
;
;(/ 
;(+((smooth identity)4.000000000133333)
;((smooth identity)(+ 4.000000000133333 dx))
;((smooth identity)(- 4.000000000133333 dx)))3)
;
;((smooth(smooth(smooth square)))2)
;((smooth(smooth(lambda (x)
;                 (/ (+ (square x)
;                       (square (+ x dx))
;                       (square (- x dx)))
;                    3))))2)
;((smooth(lambda (x)
;          (/ (+ ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) x)
;                ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) (+ x dx))
;                ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) (- x dx)))
;             3)))2)
;;3 levels expanded
;((lambda (x3)
;    (/ (+ ((lambda (x2a)
;          (/ (+ ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) x2a)
;                ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) (+ x2a dx))
;                ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) (- x2a dx)))
;             3)) x3)
;          ((lambda (x2b)
;          (/ (+ ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) x2b)
;                ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) (+ x2b dx))
;                ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) (- x2b dx)))
;             3)) (+ x3 dx))
;          ((lambda (x2c)
;          (/ (+ ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) x2c)
;                ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) (+ x2c dx))
;                ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) (- x2c dx)))
;             3)) (- x3 dx)))
;       3))2)
;
;
;    (/ (+ ((lambda (x2a)
;          (/ (+ ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) x2a)
;                ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) (+ x2a dx))
;                ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) (- x2a dx)))
;             3)) 2)
;          ((lambda (x2b)
;          (/ (+ ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) x2b)
;                ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) (+ x2b dx))
;                ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) (- x2b dx)))
;             3)) (+ 2 dx))
;          ((lambda (x2c)
;          (/ (+ ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) x2c)
;                ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) (+ x2c dx))
;                ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) (- x2c dx)))
;             3)) (- 2 dx)))
;       3)
;;sub in 2
;(/ (+ 
;          (/ (+ ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) 2)
;                ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) (+ 2 dx))
;                ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) (- 2 dx)))
;             3)
;          
;          (/ (+ ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) (+ 2 dx))
;                ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) (+ (+ 2 dx) dx))
;                ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) (- (+ 2 dx) dx)))
;             3) 
;          
;          (/ (+ ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) (- 2 dx))
;                ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) (+ (- 2 dx) dx))
;                ((lambda (x)
;                   (/ (+ (square x)
;                         (square (+ x dx))
;                         (square (- x dx)))
;                      3)) (- (- 2 dx) dx)))
;             3)) 
;       3)
;
;;sub in 2
;(/ (+ 
;          (/ (+ 
;                   (/ (+ (square 2)
;                         (square (+ 2 dx))
;                         (square (- 2 dx)))
;                      3)
;                
;                   (/ (+ (square  (+ 2 dx))
;                         (square (+  (+ 2 dx) dx))
;                         (square (-  (+ 2 dx) dx)))
;                      3)
;                
;                   (/ (+ (square (- 2 dx))
;                         (square (+ (- 2 dx) dx))
;                         (square (- (- 2 dx) dx)))
;                      3) )
;             3)
;          
;          (/ (+ 
;                   (/ (+ (square (+ 2 dx))
;                         (square (+ (+ 2 dx) dx))
;                         (square (- (+ 2 dx) dx)))
;                      3) 
;                
;                   (/ (+ (square (+ (+ 2 dx) dx))
;                         (square (+ (+ (+ 2 dx) dx) dx))
;                         (square (- (+ (+ 2 dx) dx) dx)))
;                      3) 
;                
;                   (/ (+ (square (- (+ 2 dx) dx))
;                         (square (+ (- (+ 2 dx) dx) dx))
;                         (square (- (- (+ 2 dx) dx) dx)))
;                      3) )
;             3) 
;          
;          (/ (+ 
;                   (/ (+ (square (- 2 dx))
;                         (square (+ (- 2 dx) dx))
;                         (square (- (- 2 dx) dx)))
;                      3) 
;                
;                   (/ (+ (square (+ (- 2 dx) dx))
;                         (square (+ (+ (- 2 dx) dx) dx))
;                         (square (- (+ (- 2 dx) dx) dx)))
;                      3) 
;                
;                   (/ (+ (square (- (- 2 dx) dx))
;                         (square (+ (- (- 2 dx) dx) dx))
;                         (square (- (- (- 2 dx) dx) dx)))
;                      3) )
;             3)) 
;       3)

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