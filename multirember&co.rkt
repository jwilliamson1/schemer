#lang racket


(define multirember&co
  (lambda (a l col)
    (cond ((null? l) (col '()'()))
          ((eq? a (car l))
           (multirember&co a (cdr l)
                           (lambda (newlat seen)
                             (col newlat
                                  (cons (car l) seen)))))
          (else (multirember&co a (cdr l)
                                (lambda (newlat seen)
                                  (col (cons (car l) newlat)
                                       seen)))))))

(define show-lengths
  (lambda (newlat seen)
    (cons (length newlat)(cons(length seen)'()))))

(multirember&co 'and '(the the the the and and the) show-lengths)