#lang racket
(define exact-filters '(('type1 (cons '1 '2))
                        ('type2 (cons '100 111)
                        ('type1 (cons '3 '4))
                        )))

(define result (car (group-by (λ (type) (eq? type type)) exact-filters)))

result