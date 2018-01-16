#lang racket
(define (dis text)  
  (display text)
  (newline))

(define (disp label var)
  (cons label (cons var null)))


(define (enumerate-interval start end)
  (define(iter result start)
    (let ((next (+ start 1)))
      (if(>= start end)
         result
         (iter (append result (cons next null))next))))
  (iter (cons start null) start))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

 (define empty-board
   '()
   )

(define (adjoin-position new-row k rest-of-queens)
  ;takes enumerated rows and creates a position
  (append (list new-row) rest-of-queens)
  )

(adjoin-position 1 1 empty-board)

(adjoin-position 3 2 (adjoin-position 1 1 empty-board))

(define (safe? current-col positions);receives a sequence group of potential solutions and returns t or f to filter
  
  ;null = safe
  ;element = safe
  ;pair check safety
;  (dis (cons "called positions: " (cons positions null)))
  ;the car is the only thing we care about comparing to the rest of the list
  ;i believe no matter what this function will always receive a sequence of at least 2 elements
  (let((new-row (car positions)))
;    (dis new-row)
    (define (safe-iter? preceding-positions a-prev-col)
      (if(null? preceding-positions)
         #t
         (let((preceding-row (car preceding-positions)))
           (dis (cons "new row" (cons new-row null)))
;           (dis (cons "new col" (cons current-col null)))
           (dis (cons "called positions: " (cons positions null)))
           ;(dis (cons "prededing row" (cons preceding-row null)))
           ;(dis (cons "prededing col" (cons a-prev-col null)))
           (cond
             ;((null? new-row)#t);if get to the end of sequence then we good
             ((null? preceding-row)#t)
             ;((null? (cdr preceding-positions))#t)
             ((= new-row preceding-row)#f)
             (
              ;(or
               (= (abs(- new-row preceding-row))(- current-col a-prev-col))
               ;(= (+ new-row preceding-row)(+ current-col a-prev-col))
               ;)
              #f)
             (else (safe-iter? (cdr preceding-positions)(- a-prev-col 1)))))))
    (safe-iter? (cdr positions)(- current-col 1)))
  )
  
(define queens-seq '((1 7 2 6)(6 2 7 1)))
(null?(car(car(cdr '((1 1)(() ()))))))
;(safe? 1 '((1 1)(()())))
;(safe? 2 '((2 2)(1 1)(()())))
;(safe? 2 '((3 2)(1 1)(()())))
;(safe? 2 '((1 2)(2 1)(()())))
;(safe?  4 queens-seq)

(define (queens board-size)
  ;returns a seq of all solutions
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter;returns a set of safe positions
         (lambda (positions) 
           (safe? k positions))
         (flatmap ;returns stream of unchecked new positions appended to ok'ed positions
          (lambda (rest-of-queens)
            (map (lambda (new-row);creates stream of sets of queen positions
                   (adjoin-position 
                    new-row 
                    k
                    rest-of-queens))
                 (enumerate-interval ;creates stream of vertical rows
                  1 
                board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(dis "actual queens")
(queens 6)
  
;(dis 'map-test)
;(flatmap
; (lambda (rest-of-queens)
;   (map (lambda (new-row);creates stream of sets of queen positions
;          (adjoin-position 
;           new-row 
;           1
;           rest-of-queens))
;        (enumerate-interval ;creates stream of vertical rows
;         1 
;         8))
;   )
; '((1 1)
;  (1 2)
;  (1 3)
;  (1 4)
;  (1 5)
;  (1 6)
;  (1 7)
;  (1 8)
;  (2 1)
;  (2 2)
;  (2 3)
;  (2 4)
;  (2 5)
;  (2 6)
;  (2 7)
;  (2 8)
;  (3 1)
;  (3 2)
;  (3 3)
;  (3 4)
;  (3 5)
;  (3 6)
;  (3 7)
;  (3 8)
;  (4 1)
;  (4 2)
;  (4 3)
;  (4 4)
;  (4 5)
;  (4 6)
;  (4 7)
;  (4 8)
;  (5 1)
;  (5 2)
;  (5 3)
;  (5 4)
;  (5 5)
;  (5 6)
;  (5 7)
;  (5 8)
;  (6 1)
;  (6 2)
;  (6 3)
;  (6 4)
;  (6 5)
;  (6 6)
;  (6 7)
;  (6 8)
;  (7 1)
;  (7 2)
;  (7 3)
;  (7 4)
;  (7 5)
;  (7 6)
;  (7 7)
;  (7 8)
;  (8 1)
;  (8 2)
;  (8 3)
;  (8 4)
;  (8 5)
;  (8 6)
;  (8 7)
;  (8 8)))

