#lang sicp
(define (make-tree entry left right) (cons entry (cons left  right)))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (cddr tree))
(define (entry-tree tree) (car tree))
(define lefts-left (make-tree 'c '() '()))
(define rights-left (make-tree 'q '() '()))
(define l1 (make-tree 'f lefts-left '()))
(define r1 (make-tree 'z rights-left '()))
(define t1 (make-tree 'm l1 r1))

(left-branch t1)
(right-branch t1)

t1

(define (invert-tree tree)
  (cond ((null? tree) '())
        ((cons (entry-tree tree)
               (cons (invert-tree (right-branch tree))
                     (invert-tree (left-branch tree)))))))

(invert-tree t1)
