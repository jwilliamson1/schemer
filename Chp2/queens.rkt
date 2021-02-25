#lang racket
(define (dis text)  
  (display text)
  (newline))

(define (disp label var)  
  (display(cons label (cons var null)))
  (newline))


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
  (disp "incoming positions: " positions)
  ;null = safe
  ;element = safe
  ;pair check safety
  ;the car is the only thing we care about comparing to the rest of the list
  ;i believe no matter what this function will always receive a sequence of at least 2 elements
  (let((new-row (car positions)))
;    (dis new-row)
    (define (safe-iter? preceding-positions a-prev-col)
      (if(null? preceding-positions)
         #t
         (let((preceding-row (car preceding-positions)))
           (cond
             ((null? preceding-row)#t)
             ((= new-row preceding-row)#f)
             ((= (abs(- new-row preceding-row))(- current-col a-prev-col))#f)
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

(define (safe?-true current-col positions)
  (disp "incoming positions: " positions)
  #t)

(define (queens board-size)
  ;returns a seq of all solutions
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter;returns a set of safe positions
         (lambda (positions) 
           (safe?-true k positions))
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
(length(queens 4))



(define (queens-slow board-size)
  ;returns a seq of all solutions
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter;returns a set of safe positions
         (lambda (positions) 
           (safe?-true k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position 
                    new-row
                    k
                    rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size))
         )))
  (queen-cols board-size))

(queens-slow 4)

;k not 0 so goes to filter
;filter executes flatmap which takes 1 - 8 as a sequence
;1-8 gets applied to lambda as new row
;for each one of these numbers 1-8 the lambda's
;map is called which makes a recursive call to queen-cols k-1
;so this is k * n times for however big k is
;each one of these calls to queens-slow will generate n recursive calls to queens cols k-1

;for 2 call flat map twice
;each flatmap calls lambda new row twice on queen cols - 1 = k is 1
;each flatmap calls lambda new row twice on queen cols - 1 = k is 0 so this is 4 calls to k=0
;return nulls to k-2 flatmap and map enumerate intervals to it ()()()()

;for 3 call flatmap thrice
;each flatmap calls lambda new row 3 times on queen cols 2 | 3 flatmaps waiting
;each flatmap calls lambda new row 3 times on queen cols 1 | 3 flatmaps waiting and 9 total flatmaps
;each flatmap calls lambda new row 3 times on queens col 0 | 3 flat maps waiting and 27 total flatmaps
;each flatmap on a group of 9 3 times = 27
;each flatmap on a group of 9 3 times = 27

;for 4 calls flap map gets called 4 times
;each flatmap calls lambda new row 4 times on queen cols 3 | 
;each flatmap calls lambda new row 4 times on queen cols 2 | 16 total flatmaps
;each flatmap calls lambda new row 4 times on queen cols 1 | 64 total flat maps
;each flat map calls λ new row 4 times on queen cols 0 | 256 total flat maps
; 4 * 256 = 1024
  
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

