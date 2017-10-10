#lang racket


(define (cont-frac n d k)
  (if (= k 0)
      (/ (n k) (d k))
      (+ (d k) (/  (n k)
              (cont-frac n d (- k 1))))))

(define (cont-frac-iter n d k)
  (define (iter n d k a)
    (if (= k 0)
        a
        ;(/ a (/ (n k)(/ (d k))))
        (iter n d (- k 1) (+ (d k)(/ (n k)(/(n k)(d k)))))))
  (iter n d k 0))

(cont-frac-iter (lambda (i) 1.0)
           (lambda (i) 1.0)
           1)