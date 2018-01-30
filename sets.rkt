#lang racket
;
;(define (element-of-set? x set)
;  (cond ((null? set) #f)
;        ((equal? x (car set)) #t)
;        (else (element-of-set? x (cdr set)))))
;
;(define (adjoin-set x set)
;  (if (element-of-set? x set)
;      set
;      (cons x set)))
;
;(define (intersection-set set1 set2)
;  (cond ((or (null? set1) (null? set2)) 
;         '())
;        ((element-of-set? (car set1) set2)
;         (cons (car set1)
;               (intersection-set (cdr set1) 
;                                 set2)))
;        (else (intersection-set (cdr set1) 
;                                set2))))
;
;(define (union-set set1 set2)
;  (cond ((null? set1)set2)        
;        ((element-of-set? (car set1) set2)
;         (union-set (cdr set1) set2))
;        (else (cons (car set1)
;                    (union-set (cdr set1)
;                                      set2)))))
;(element-of-set? 1 '(1 2 3))
;(union-set '(10 3 5)'(1 2 3))

(define set1 '(4 9 8 6 43 4))
(define set2 '(10 6 4 89 2 3 5 43 4))
;9 8 10 6 4 89 2 5 43 4
(define (element-of-set?* x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set?* x (cdr set)))))

(element-of-set?* 4 '(9 8 6 43 4 4))
;O(n) but potential worse due to more elements
(define (adjoin-set* x set)
  (cons x set))
;O(1) vs O(n)
(adjoin-set* 5 set1)

(define (intersection-set* set1 set2)
  (cond((or(null? set1)(null? set2))
        '())
       ((element-of-set?* (car set1) set2)
        (cons (car set1)
              (intersection-set* (cdr set1)
                                 set2)))
       (else (intersection-set* (cdr set1)
                                set2))))
(intersection-set* set1 set2)
;O(n^2)
(define(union-set* set1 set2)
  (append set1 set2))
(union-set* set1 set2)
;O(n)

(define (adjoin-set x set)
  (cond((null? set)(list x))
       ((< x (car set))(cons x set))
       ((= x (car set))set)
       (else (cons (car set)(adjoin-set x (cdr set))))))
(displayln "test adjoin-set") 
(adjoin-set 3 '(1 2 4))
(adjoin-set 3 '(1 2 5))
(adjoin-set 6 '(1 2 5))
(adjoin-set 1 '(1 2 5))
(adjoin-set 1 '(2 3 4))
(adjoin-set 2 '(2 3 4))
(adjoin-set 1 '(3 4 5))
(adjoin-set 2 '(3 4 5))
(adjoin-set 2 '())

;(define (union-set set1 set2)
;  (cond((null? set1)set2)
;       (else (union-set (cdr set1)(adjoin-set (car set1)set2)))))
;still O(n^2) just slightly better


;(define (union-set set1 set2)
;  (if
;   (let ((x1 (car set1))(x2 (car set2)))
;     (cond((= x1 x2)
;           (cons x1 (union-set (cdr set1)(cdr set2))))
;          ((and(< x1 x2)(or(null? (cdr set1))(< x2 (cadr set2))))
;           (cons x1 (cons x2 (union-set (cdr set1)(cdr)

(define (union-set set1 set2)
  (cond((null? set1)set2)
       ((null? set2)set1)
       (else
        (let ((x1 (car set1))(x2 (car set2)))
          (cond((= x1 x2)
                (cons x1 (union-set (cdr set1)(cdr set2))))
               ((and(< x1 x2))
                (cons x1 (union-set (cdr set1) set2)))
               ((and(> x1 x2))
                (cons x2 (union-set set1 (cdr set2)))))))))

(displayln "test union-set")                                        
(union-set '(1 3 5 8)'(2 4 6 7))
(union-set '(1 3 5)'(1 2 3 4 5 6))
(union-set '(1 2 3 4 5 6)'(2 4 6))
(union-set '(1 2 3)'(4 5 6))
(union-set '(2 3 4 5 6)'(1 2 3 7))

(define (disjoin set1 set2)
  ;numbers that are not in both sets
  (cond((null? set1)set2)
       ((null? set2)set1)
       (else
        (let((x1 (car set1))(x2 (car set2)))
          (cond((= x1 x2)
               (disjoin (cdr set1)(cdr set2)))
               ((< x1 x2)
                (cons x1(disjoin(cdr set1)set2)))
               (else(cons x2(disjoin set1 (cdr set2)))))))))

(disjoin '(1 2 3)'(1 3 4))
(disjoin '(1 2 3 6)'(1 3 4))
(disjoin '(1 2 3 6)'(1 3 4 5 7))

(define(difference set1 set2)
    ;only the numbers from the first set not in the second
    (cond((null? set1)'())
       ((null? set2)set1)
       (else
        (let((x1 (car set1))(x2 (car set2)))
          (cond((= x1 x2)
               (difference (cdr set1)(cdr set2)))
               ((< x1 x2)
                (cons x1(difference(cdr set1)set2)))
               (else(difference set1 (cdr set2))))))))
(difference '(1 2 3)'(1 3 4))
(difference '(1 2 3 6)'(1 3 4))
(difference '(1 2 3 5 6)'(1 3 4 5 7))
(difference '(1 2 3 5 6 7)'(1 3 4 7))
(append '() (list 1) '() (list 2))
               
        