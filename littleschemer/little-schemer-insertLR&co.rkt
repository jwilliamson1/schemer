#lang racket
(define atom?
  (lambda (x)
    (and (not (null? x))
         (not (pair? x)))))

(define multiInsertLR
  (lambda (new oldL oldR lat)
    (cond ((null? lat) '())
          ((eq? (car lat) oldL)
           (cons new (cons (car lat) (multiInsertLR new oldL oldR (cdr lat)))))
          ((eq? (car lat) oldR)
           (cons (car lat) (cons new (multiInsertLR new oldL oldR (cdr lat)))))
          (else (cons (car lat) (multiInsertLR new oldL oldR (cdr lat)))))))

(multiInsertLR 'bright 'shining 'illuminated '(the shining knight stood tall in the illuminated hall did i mention it
                                                   was shining and the hall was illuminated ?))

(define multiInsertLR&co
  (lambda (new oldL oldR lat col)
    (cond ((null? lat) (col '() 0 0))
          ((eq? (car lat) oldL)
           (multiInsertLR&co new oldL oldR (cdr lat)
                             (lambda (newlat left right)
                               (col(cons new (cons (car lat) newlat)) (add1 left) right))))
          ((eq? (car lat) oldR)
           (multiInsertLR&co new oldL oldR (cdr lat)
                             (lambda (newlat left right)
                               (col(cons (car lat) (cons new newlat)) left (add1 right)))))
          (else (multiInsertLR&co new oldL oldR (cdr lat)
                                  (lambda (newlat left right)
                                    (col (cons (car lat) newlat) left right)))))))

(multiInsertLR&co 'bright 'shining 'illuminated '(the shining knight stood tall in the illuminated hall did i mention it
                                                   was shining and the hall was illuminated ?)
                  (lambda (newlat left right)
                    (list newlat left right)))

(even? 5)
(even? 4)

(define evens-only*
  (lambda (l)
    (cond ((null? l) '())
          ((atom? (car l))
           (cond ((even? (car l))
                  (cons (car l) (evens-only* (cdr l))))
                 (else (evens-only* (cdr l)))))
          (else (cons (evens-only* (car l))
                      (evens-only* (cdr l)))))))

(even? (car '(4)))
(evens-only* '(10))
(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2) )

(define evens-only*&co
  (lambda (l col)
    (cond ((null? l) (col '() 1 0))
          ((atom? (car l))
           (cond ((even? (car l))
                  (evens-only*&co (cdr l)
                                  (lambda (evens prod sum)
                                    (col (cons (car l) evens)
                                         (* (car l) prod)
                                         sum))))
                 (else (evens-only*&co (cdr l)
                                       (lambda (evens prod sum)
                                         (col evens
                                              prod
                                              (+ (car l) sum)))))))
          (else (evens-only*&co (car l)
                                (lambda (aevens aprod asum)
                                  (evens-only*&co (cdr l)
                                                  (lambda (bevens bprod bsum)
                                                    (col (cons aevens bevens)
                                                         (* aprod bprod)
                                                         (+ asum bsum))))))))))

(define simple-col (lambda(evens prod sum)(cons evens (cons prod (cons sum '())))))
(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) simple-col)