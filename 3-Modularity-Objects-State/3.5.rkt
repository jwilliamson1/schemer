#lang racket
(define (rand-update x) (random (expt 2 31)))

(rand-update 4)

(define (random-init) (rand-update 0))
 
(define rand
  (let ((x (random-init)))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials 
                          cesaro-test))))
(define (cesaro-test)
   (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) 
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) 
                 trials-passed))))
  (iter trials 0))

(estimate-pi 10000)

(define (inside-circle? x y)
  (<= (+ (expt (- x 5) 2)
         (expt (- y 7) 2))
      (expt 3 2)))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range))))

(define (area x1 x2 y1 y2)
  (* (- x2 x1)
     (- y2 y1)))

(area 0 4 0 5)
(area 2 8 4 10)

(random-in-range 1 100)

(define (predicate-test x1 x2 y1 y2 P)
  (P (random-in-range x1 x2) (random-in-range y1 y2)))

(predicate-test 1 10 1 10 inside-circle?)
               
(inside-circle? 1 10)
(inside-circle? 4 6)

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (predicate-test)
    (P (random-in-range x1 x2)(random-in-range y1 y2)))
  
  (/ (*
   (area x1 x2 y1 y2)
     (monte-carlo trials
                  predicate-test)) 9.0))

(estimate-integral inside-circle? 2 8 4 10 1000000)
  