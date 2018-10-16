#lang racket

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (eqan? x1 x2)
  (cond ((and (number? x1)(number? x2))
         (= x1 x2))
        ((or (number? x1)(number? x2))#f)
        (else (eq? x1 x2))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1 )(null? l2)) #t)
      ((or (null? l1)(null? l2)) #f)   
      (else
       (and (equal? (car l1) (car l2))
            (equal? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (s1 s2)
    (cond((and (atom? s1)(atom? s2))
          (eqan? s1 s2))
         ((or (atom? s1)(atom? s2)) #f)
         (else (eqlist? s1 s2)))))

(display "should be #t")(eqlist? '(((test)) test) '(((test)) test))
(display "should be #t")(eqlist? '(test (test)) '(test (test)))
(display "should be #f")(eqlist? '(test (test)) '(test test))
(display "should be #t")(eqlist? '() '())
(display "should be #f")(eqlist? '(a) '())
(display "should be #f")(eqlist? '() '(a))
(display "should be #f")(eqlist? '(a v) '())
(display "should be #f")(eqlist? '() '(a v))
(display "should be #t")(eqlist? '(a b) '(a b))
(display "should be #t")(eqlist? '(strawberry ice cream) '(strawberry ice cream))
(display "should be #f")(eqlist? '((test) test2) '(test test2))
;(display "should be #f")(eqlist? '(test test2) '((test) test2))



(equal? '(a b)'(a b))
(equal? '(a b)'(a b c))
(equal? '(a bc)'(a b))
(equal? 'a'(a b))
(equal? '(a b) 'b)
(equal? '(a (b))'(a (b)))
(equal? '((a) b)'((a) b c))
(equal? '(a bc)'(a b))
(equal? 'a'(a b))
(equal? '(a b) 'b)
(equal? '() '())
(equal? 'b '())
(equal? '(a b) '())

(define rember
  (lambda (s l)
    (cond ((null? l) '())
          ((equal? (car l) s)
           (rember s (cdr l)))
          (else (cons (car l)(rember s (cdr l)))))))

(rember 'a '(a b c))
(rember '(a b c) '(z b (a b c) x 4))
           