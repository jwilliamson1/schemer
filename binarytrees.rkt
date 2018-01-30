#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append 
       (tree->list-1 
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1 
              (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (displayln tree)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

;(define right-tree (make-tree  1 '() (make-tree  2 '() (make-tree  3 '() '()))))
;right-tree
;(define balanced-tree (make-tree 2 (make-tree 1 '()'())(make-tree 3 '()'())))
;(define left-tree (make-tree 3 (make-tree 2 (make-tree 1 '()'())'())'()))

(define deep-right-tree (make-tree 3 (make-tree 1 '()'())(make-tree 7 (make-tree 5 '()'())(make-tree 9 '() (make-tree 11'()'())))))
(define left-weight-tree (make-tree 7 (make-tree 3 (make-tree 1 '()'())(make-tree 5 '()'()))(make-tree 9 '()(make-tree 11 '() '()))))
(define right-weight-tree (make-tree 5 (make-tree 3 (make-tree 1 '()'())'())(make-tree 9 (make-tree 7 '()'())(make-tree 11 '()'()))))

deep-right-tree

(tree->list-1 deep-right-tree)
(tree->list-1 right-weight-tree)
(tree->list-1 left-weight-tree)
;O(2^n)

(tree->list-2(make-tree 7 (make-tree 3 (make-tree 1 '()'())(make-tree 5 '()'()))'()))