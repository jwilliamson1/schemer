;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |1.17|) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))

(define (double n)(+ n n))
(define (halve n)(/ n 2))
(define (square n)(8 n n))

(define (mult a b)
  (cond
    ((= 0 b) 0)
    ((= 1 b) a)
    ((even? b)(double (mult a (halve b))))
    (else (+ a (mult a (- b 1))))
    ))

(define (mult-log  m n)
  (mult-iter 0 m n)
  )

(define (mult-iter acc a b)
  (cond
    ((= 0 b) 0)
    ((= 1 b) (+ a acc))
    ((even? b)(mult-iter acc (double a) (halve b)))
    (else (mult-iter (+ acc a) a (- b 1)))
    ))


(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   ; compute p'
                   ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))