#lang sicp

(define (contains? list item)
  (if (null? list) #f
      (if (eq? (car list) item) #t
          (contains? (cdr list) item))))


(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(pair? (cons 1 2))
;#t
(pair? (cdr (cons 1 2)))
;#f
(pair? (cdr (cons 1 '())))
;#f

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define (count-pairs2 x)
  (define (zero) 0)
  (define (iter x seen cont)
    ;(display (list x cont))
    (if (or (not (pair? x)) (memq x seen)) (cont)
        (begin
          (append! seen (list x))
          (+ 1 (iter (car x) seen (lambda rest
                                    (iter (cdr x) seen cont)))))))
  (iter x (list (cons 'head '())) (lambda rest 0)))

(count-pairs2 (cons 'a 'b))
(count-pairs2 (cons (cons 'a 'b) 'c))
(count-pairs2 (cons 'a (cons 'b 'c)))
(count-pairs2 (cons (cons 'a 'b)(cons 'x 'y)))
        

(count-pairs2 (cons (cons 'a '())(cons 'b '())))

(define reused (cons 'x 'x))

(define four (cons 'a (cons reused reused)))

(count-pairs2 four)

(define reused4 (cons 'x 'x))

(define reused2 (cons reused4 reused4))

(define seven (cons reused2 reused2))

(set-cdr! reused4 seven)
;never returns!

(count-pairs2 seven)

(define lst (list reused reused4 reused2))

(define never-used (cons 'a 'a))

(contains? lst reused)
(contains? lst never-used)


