#lang lazy
(define fizz (cons "" (cons "" (cons "fizz" fizz))))
fizz
(list-ref fizz 2)
(list-ref fizz 5)

(define buzz (cycle "" "" "" "" "buzz"))
(list-ref buzz 4)
(list-ref buzz 9)

(define ones (cons 1 ones))
(list-ref ones 4)

(define integers (cons 1 (map + integers ones)))
(list-ref integers 4)

(define resonance (map string-append fizz buzz))
(list-ref resonance 0)
(list-ref resonance 2)
(list-ref resonance 4)
(list-ref resonance 5)
(list-ref resonance 8)
(list-ref resonance 14)
(list-ref resonance 15)

(define (merge-results fb i)
  (if (eq? (string-length fb) 0) i
      fb))

(define fizzbuzz (map merge-results resonance integers))
(list-ref fizzbuzz 0)
(list-ref fizzbuzz 2)
(list-ref fizzbuzz 4)
(list-ref fizzbuzz 5)
(list-ref fizzbuzz 14)
(list-ref fizzbuzz 15)