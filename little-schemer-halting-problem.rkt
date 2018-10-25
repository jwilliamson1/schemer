#lang racket

(define pick
  (lambda (pos lat)
    (cond
      ((eq? pos 1)(car lat))
      (else (pick (sub1 pos)(cdr lat))))))

(pick 1 '(a b c))
(pick 2 '(a b c))

(define looking
  (lambda (a choice lat)
    (keep-looking a (pick choice lat) lat)))

(define keep-looking
  (lambda (a pos lat)
    (cond
      ((eq? (pick pos lat) a) #t)
      (else (looking a pos lat)))))

(looking  'caviar 1 '(6 2 4 caviar 5 7 3))