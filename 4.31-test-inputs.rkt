#lang racket
(define (unless pred con alt)(if pred alt con))
(unless (> 10 5) (/ 1 0) 5)
(define (unless pred (con lazy) alt)(if pred alt con))
(unless (> 10 5) (/ 1 0) 5)
(define (unless pred (con lazy-memo) alt)(if pred alt con))
(unless (> 10 5) (/ 1 0) 5)
(define (fib (n lazy-memo))
  (let fib-iter (((a lazy-memo) 1) ((b lazy-memo) 0) (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) 
                  a 
                  (- count 1)))))

(define (fib n)
  (if (= n 1)
      0
      (if (= n 2)
          1
          (+ (fib (- n 1)) (fib (- n 2))))))

(define (fib (n lazy))
  (if (= n 1)
      0
      (if (= n 2)
          1
          (+ (fib (- n 1)) (fib (- n 2))))))

(define (fib (n lazy-memo))
  (if (= n 1)
      0
      (if (= n 2)
          1
          (+ (fib (- n 1)) (fib (- n 2))))))