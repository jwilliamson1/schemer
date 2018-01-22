

(cons 'a 'b)
(cons 'a 2)
(define b 3)

(cons 'a b)

(car '(a b c))

(cadddr (quote(a b c 3)))

(list 'a 'b 'c 'abc)
;'(a b c)
(list (list 'george))
;'((george))
(cdr '((x1 x2) (y1 y2)))
;'((y1 y2))
(cadr '((x1 x2) (y1 y2)))
;'(y1 y2)
(pair? (car '(a short list)))
;false
(memq 'red '((red shoes) (blue socks)))
;false
(memq 'red '(red shoes blue socks))
;true

;(define (equal? a b)
;  (or
;   (eq? a b)
;   (and
;    (pair? a)
;    (pair? b)
;    (equal? (car a)(car b))
;    (equal? (cdr a)(cdr b)))))

(car '(a b c))
(car (quote (a b c)))
(car '(list 'quote a b c))

(car '(quote a))

