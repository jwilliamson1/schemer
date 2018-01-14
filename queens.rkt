#lang racket
(define (dis text)  
  (display text)
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
   '())

(define (adjoin-position new-row k rest-of-queens)
  ;takes enumerated rows and creates a position
  (cons rest-of-queens (cons(list new-row k)null))
  )

(adjoin-position 1 1 empty-board)

(define (safe? current-col positions) ;ie 1 (1 1)(()()) or 2 (2 2)(1 1)(()())
  ;receives streams of conditions like  (1 1) (() ())
  ;determines if current position is horizotonal - first elements match
  ;or diagnal -increased linear - to another position
;  (if(null? (car positions))#t     
;     (let ((attempted-row (car (car positions))))
;       
;       (define (iter  lower-poses)
;         (let ((prev-row (car (car lower-poses)))
;               (a-prev-col (car(cdr (car lower-poses)))))
;           (cond ((null? prev-row)#t)
;                 ((= attempted-row prev-row)#f)
;                 ((or(= (- attempted-row prev-row)(- current-col a-prev-col))
;                     (= (+ attempted-row prev-row)(+ current-col a-prev-col)))#f)
;                 (else (iter(cdr lower-poses)))
;                 )))
;       (iter (cdr positions))))
  #t
  )

(null?(car(car(cdr '((1 1)(() ()))))))
(safe? 1 '((1 1)(()())))
(safe? 2 '((2 2)(1 1)(()())))
(safe? 2 '((3 2)(1 1)(()())))
(safe? 2 '((1 2)(2 1)(()())))
(define (queens board-size)
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



(map (lambda (new-row)
        (adjoin-position 
         new-row 
         1
         empty-board))
      (enumerate-interval 
       1 
       8))
(queens 2)