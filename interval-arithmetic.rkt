#lang racket

(define (make-interval a b)
  (if(< a b)
     (cons a b)
     (display "ERROR: arg1 must be < arg2")))

(define int-x (make-interval 1 5))
(define int-y (make-interval 2 7))
  

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

  
(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

(lower-bound(add-interval int-x int-y))
(upper-bound(add-interval int-x int-y))

(define (old-mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (mul-interval x y)
  (let ((xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y)))
    (cond
      ((and(neg? xl)(neg-or-0? xu)(neg-or-0? yl)(pos? yu))     
       (make-interval(* xl yu)(* xl yl)))
      ((and(neg? xl)(neg-or-0? xu)(neg-or-0? yl)(neg-or-0? yu))     
       (make-interval(* xu yu)(* xl yl)))
      ((and(neg? xl)(neg-or-0? xu)(pos? yl)(pos? yu))     
       (make-interval(* xl yu)(* xu yl)))
      ((and(neg-or-0? xl)(pos? xu)(pos? yl)(pos? yu))     
       (make-interval(* xl yu)(* xu yu)))
      ((and(pos? xl)(pos? xu)(pos? yl)(pos? yu))     
       (make-interval(* xl yl)(* xu yu)))
      ((and(neg-or-0? xl)(pos? xu)(neg-or-0? yl)(neg-or-0? yu))     
       (make-interval(* xu yl)(* xl yl)))
      ((and(neg-or-0? xl)(pos? xu)(neg-or-0? yl)(pos? yu))     
       (let ((l(min(* xl yu)(* xu yl)))
             (u(max(* xl yl)(* xu yu))))
         (make-interval l u)))
      )))

(define (pos? x)
  (> x 0))

(define (neg? x)
  (< x 0))

(define (neg-or-0? x)
  (<= x 0))

(pos? 5)
(pos? -5)
(pos? 0)

 (define (lower-pos? int)
  (positive?  (lower-bound int)))

(lower-pos? int-x)

(define (upper-pos? int)
  (positive? (upper-bound int)))

(upper-pos? int-y)

(display "ok ")(lower-bound(mul-interval int-x int-y))
(display "ok ")(upper-bound(mul-interval int-x int-y))

(define (div-interval x y)
  (cond
    ((= (upper-bound y) 0) (display "ERROR divide by zero")(newline))
    ((= (lower-bound y) 0) (display "ERROR divide by zero")(newline))
    (else
     (mul-interval x    
                   (make-interval
                    (/ 1.0 (upper-bound y)) 
                    (/ 1.0 (lower-bound y)))))))

(lower-bound(div-interval int-x int-y))
(upper-bound(div-interval int-x int-y))


(lower-bound(div-interval (make-interval 1 5)(make-interval 3 7)))
(upper-bound(div-interval (make-interval 1 5)(make-interval 3 7)))



(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (lower-bound y))
                 (-(upper-bound x)
                   (upper-bound y))))

(sub-interval int-x int-y)
(sub-interval int-x int-y)

(define(width int)
  (/(-(upper-bound int)(lower-bound int))2))

(define sum1 (make-interval 1 20))
(define sum2 (make-interval 21 40))

(display "should be 9.5")(width sum1)
(display "should be 9.5")(width sum2)

(define (sum-of-widths i1 i2)
  (+ (width i1)(width i2)))

(define (mul-of-widths i1 i2)
  (* (width i1)(width i2)))

(define (div-of-widths i1 i2)
  (/ (width i1)(width i2)))

(sum-of-widths sum1 sum2)
(display "above and below should be equal")(newline)
(width (add-interval sum1 sum2))

(mul-of-widths sum1 sum2)
(display "above is wrong because mul should be appiled first")(newline)
(width (mul-interval sum1 sum2))

(div-of-widths sum1 sum2)
(display "above is wrong because div should be appiled first")(newline)
(width (div-interval sum1 sum2))
;(mul-interval (make-interval 1 .2)(make-interval 2 .7))

(define neg1 (make-interval -15 -11))
(define neg2 (make-interval -10 6))

(define neg3 (make-interval -15 -11))
(define neg4 (make-interval -10 -6))

;(define neg5 (make-interval -15 -11))
;(define neg6 (make-interval 10 -6))

(define neg7 (make-interval -15 -11))
(define neg8 (make-interval 6 10))

;(define neg9 (make-interval 15 -11))
;(define neg10 (make-interval 6 10))

(define neg11 (make-interval -15 11))
(define neg12 (make-interval 6 10))

(define neg13 (make-interval 11 15))
(define neg14 (make-interval 6 10))

(define neg15 (make-interval 0 5))
(define neg16 (make-interval 6 10))

(define neg17 (make-interval -5 0))
(define neg18 (make-interval -6 10))

(define neg19 (make-interval -10 12))
(define neg20 (make-interval -10 10))



(old-mul-interval neg1 neg2) 
(mul-interval neg1 neg2)
(old-mul-interval neg3 neg4) 
(mul-interval neg3 neg4)
;(old-mul-interval neg5 neg6) 
;(mul-interval neg5 neg6)
(old-mul-interval neg7 neg8) 
(mul-interval neg7 neg8)
;(old-mul-interval neg9 neg10) 
;(mul-interval neg9 neg10)
(old-mul-interval neg11 neg12) 
(mul-interval neg11 neg12)
(old-mul-interval neg13 neg14) 
(mul-interval neg13 neg14)
(old-mul-interval neg15 neg16) 
(mul-interval neg15 neg16)
(old-mul-interval neg17 neg18) 
(mul-interval neg17 neg18)
(old-mul-interval neg19 neg20) 
(mul-interval neg19 neg20)
