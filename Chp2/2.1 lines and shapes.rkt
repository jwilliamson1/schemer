#lang sicp
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

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (gcd a b)
   (if (= b 0)
       a
       (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (if
     (and(< d 0)(> n 0))(cons(/ (* -1 n) g)
                             (/ (* -1 d) g))
    (cons (/ n g) 
          (/ d g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))



(define one-half (make-rat 1 2))
(print-rat one-half)
;1/2

(define one-third (make-rat 1 -3))
(print-rat
 (add-rat one-half one-third))
;5/6

(print-rat
 (mul-rat one-half one-third))
;1/6

(print-rat
 (add-rat one-third one-third))
;6/9

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


(define (midpoint-segment seg)
  (make-point(/ (+ (x-point (start-segment seg))(x-point (end-segment seg))) 2)
             (/ (+ (y-point (start-segment seg))(y-point (end-segment seg))) 2)))

(define pointA (make-point 1 2))
(define pointB (make-point 2 5))
;(x-point pointA)
(x-point pointA)
(y-point pointA)
(x-point pointB)
(y-point pointB)

(define midAB (midpoint-segment (make-segment pointA pointB)))

(print-point midAB)

(define (make-rect top-left bottom-right)
  (cons top-left bottom-right))

(define (make-rect-seg wide long)
  (cons (end-segment wide)
        (end-segment long)))

(define (top-left rect)
  (car rect))

(define (bottom-right rect)
  (cdr rect))

(define (perimeter rect)
  (+ (* 2(- (x-point (bottom-right rect))(x-point (top-left rect))))
     (* 2(- (y-point (top-left rect ))(y-point (bottom-right rect))))))

(define (area rect)
  (* (- (x-point (bottom-right rect))(x-point (top-left rect)))
     (- (y-point (top-left rect ))(y-point (bottom-right rect)))))

(define myRect (make-rect (make-point 1 5)(make-point 9 1)))
(define mySegRect (make-rect-seg (make-segment (make-point 0 0)(make-point 0 5) )(make-segment (make-point 0 0)(make-point 5 0))))


(top-left myRect);1,5
(bottom-right myRect);5,1

(x-point (bottom-right myRect));x BR:  8
(x-point (top-left myRect));x TL: 1


(y-point (bottom-right myRect));x BR:  1
(y-point (top-left myRect));x TL: 5

(perimeter myRect)
(area myRect)

(perimeter mySegRect)
(area mySegRect)

