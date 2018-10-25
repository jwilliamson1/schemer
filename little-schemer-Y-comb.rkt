#lang racket

(define eternity
  (lambda (x)
    (eternity x)))

((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else ( add1 ( length ( cdr l)))))))
 eternity)

; now give it an eternity empty list
(((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else ( add1 ( length ( cdr l)))))))
 eternity) '())

(((lambda (mk-length)    
    (displayln (list "mk-length: " mk-length))    
    (mk-length mk-length))
   (lambda (mk-length2)
     (lambda (l)
       (displayln (list "l: " l))
       (displayln (list "inner mk-length2: " mk-length2))
       (cond         
         (( null? l) 0)
         (else (add1
                 ((mk-length2 eternity)
                   (cdr l))))))))
  '(apples))

((lambda (l)
   (cond         
     ((null? l) 0)
     (else (add1
            (((lambda (mk-length)
                (lambda (l)
                  (cond         
                    ((null? l) 0)
                    (else (add1
                           ((mk-length eternity)
                            (cdr l))))))) eternity)
             (cdr l))))))
 '(apples))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
    (lambda (l)
      (cond         
        ((null? l) 0)
        (else (add1
               ((mk-length mk-length)
                (cdr l)))))))
 '(apples bananas oranges))

