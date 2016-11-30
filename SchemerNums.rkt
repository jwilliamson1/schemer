#lang racket
;string functions
(define firsts
  (lambda (l)
    (cond
      [(null? l)(quote())]
      [else (cons (car (car l))
                  (firsts (cdr l)))])))
                            
(define atom?
(lambda (x)
(and (not (pair? x)) (not (null? x)))))
;lat returns true if l is a list of atoms
(define lat?
  (lambda (l)
  (cond
    [(null? l) #t]
    [(atom? (car l)) (lat? (cdr l))]
    [else #f]
    )))
;number functions
(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define add
  (lambda (n m)
    (cond
      [(zero? m) n]
      [else (add1 (add n (sub1 m)))]
      )))

(define subt
  (lambda (n m)
    (cond
      [(zero? m) n]
      [else (sub1 (subt n (sub1 m)))]
      )))

(define multi
  (lambda (n m)
    (cond
      [(zero? m) 0]
      [else (+ n (multi n (sub1 m)))]
      )))

(define divi
  (lambda (n m)
    (cond
      [(zero? m) 0]
      [(< (subt n m) 0) 0]
      [(<= n 0) 0]
      [else (add1 (div (subt n m) m))])))



(define length
  (lambda (lat)
    (cond
      [(null? lat) 0]
      [else (add1(length(cdr lat)))])))

(define pick
    (lambda (n lat)
      (cond
        [(zero? (sub1 n))(car lat)]
        [else (pick (sub1 n)(cdr lat))])))

(define tup+
  (lambda (tup1 tup2)
    (cond
      [(and(null? tup1)(null? tup2)) (quote ())]
      [(null? tup1) tup2]
      [(null? tup2) tup1]
      [else (cons (add (car tup1)(car tup2))(tup+ (cdr tup1) (cdr tup2)))])))
(define t1 (list 10 20 30 50 60 70))
(define t2 (list 3 4 5 6))
(define t3 (list 100 200 300))
;tests
;(tup+ t1 t3)
;(tup+ t2 t3)
;(tup+ t2 t1)
(define grtr
  (lambda (n m)
    (cond
      [(zero? n) #f]
      [(zero? m) #t]
      [else (grtr(sub1 n)(sub1 m))])))
;(grtr 4 6)
;(grtr 3 1)
;(grtr 1 1)

(define less
  (lambda (n m)
    (cond
      [(zero? m) #f]
      [(zero? n) #t]
      [else (less(sub1 n)(sub1 m))])))
;(less 4 6)
;(less 3 1)
;(less 1 1)
(define eq?
  (lambda (n m)
    (cond
    [(and(zero? n)(zero? m))]
    [(or(zero? n)(zero? m)) #f]
    [else (eq? (sub1 n)(sub1 m))])))
;(eq? 1 3)
;(eq? 3 1)
;(eq? 0 1)
;(eq? 1 1)
;(eq? 0 0)
    
(define div
  (lambda (n m)
    (cond
      [(eq? m 0) "Err:div by 0"]
      [(less n m) 0]
      [(add1 (div (subt n m) m))])))
    (div 45 9)
(div 45 5)
(div 45 1)
(div 45 0)
