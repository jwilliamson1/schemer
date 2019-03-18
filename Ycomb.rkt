#lang racket

(define make-fact
  (lambda (func)
    ((lambda (fact)
       (lambda (n)
         (if (= 1 n) 1
             (* n (fact (- n 1))))))
     (lambda (x)((func func) x)))))

(define make-fact2
  (lambda (fact)
       (lambda (n)
         (if (= 1 n) 1
             (* n (fact (- n 1)))))))

;((make-fact2 (lambda (x)((make-fact2 make-fact2) x)))3)

(define (eternity x)
    (eternity x))

;((make-len make-len) 4)

(define not-really-Y
  (lambda (make-fact)
    (make-fact make-fact)))

(define fact-Y
  (not-really-Y make-fact))

(fact-Y 5)

(define (fact n)
  (if (= 1 n) 1
      (* n (fact (- n 1)))))

;(fact 3)

(((lambda (make-fact)
    (make-fact make-fact))
  (lambda (func)
    ((lambda (fact)
       (lambda (n)
         (if (= 1 n) 1
             (* n (fact (- n 1))))))
     (lambda (x)((func func) x)))))5)

(lambda (le)
   ((lambda (make-fact)
      (make-fact make-fact))
    (lambda (func)
      (le
       (lambda (x)((func func) x))))))

(define Y (lambda (le)
   ((lambda (make-fact)
      (make-fact make-fact))
    (lambda (func)
      (le
       (lambda (x)((func func) x)))))))

((Y (lambda (fact)
       (lambda (n)
         (if (= 1 n) 1
             (* n (fact (- n 1)))))))5)