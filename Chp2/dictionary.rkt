#lang racket
(define(make-entry dictionary key value)
  (if(null? dictionary)
     (cons(cons key (cons value null))null)
     (append dictionary (cons(cons key (cons value null))null))))

(define dict (make-entry(make-entry(make-entry '() 'A 'red) 'B 'blue) 'C 'yellow))

(make-entry '() 'Z 'zebra)

(define (get-value key dictionary)
  (cond((null? dictionary)null)
       ((eq? key (caar dictionary))(cadar dictionary))
       (else (get-value key (cdr dictionary)))))

(get-value 'C dict)

dict