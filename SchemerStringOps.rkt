#lang racket

(define atom?
  (lambda (l)
    (cond [(or (null? l)(pair? l)) #f]
          [else #t])))
          
    
(define lat?
  (lambda (l)
    (cond
      [(null? l) #t]
      [(atom? (car l)) (lat? (cdr l))]
      [else #f]
      )))
(define multiSubst
  (lambda (lat old new)
    (cond
      [(null? lat) (quote())]
      [(eq?(car lat) old)
       (cons new (multiSubst(cdr lat)))]
      [else cons (car lat)
            (multiSubst (cdr lat))]
      )))
       
