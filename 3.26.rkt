#lang sicp
(define (entry tree) (caar tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (make-tree-node x) (make-tree x '() '()))

(define test-tree-lbranch (make-tree (cons 3 'three) '() '()))
(define test-tree-rbranch (make-tree (cons 7 'seven) '() '()))
(define test-tree-root(make-tree (cons 4 'four) test-tree-lbranch test-tree-rbranch))
(entry test-tree-root)
(left-branch test-tree-root)
(right-branch test-tree-root)

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? 
          x 
          (left-branch set)))
        ((> x (entry set))
         (element-of-set? 
          x 
          (right-branch set)))))

;(element-of-set? 3 test-tree-root)
;element-of-set? 4 test-tree-root)



(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((or (null? records) (null? (car records))) false)
        ((equal? key (caar records))
         (car records))
        ((< key (caar records))
         (assoc key (left-branch records)))        
        (else (assoc key (right-branch records)))))

test-tree-root
(assoc 3 test-tree-root)
(assoc 7 test-tree-root)
(assoc -4 test-tree-root)

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) 
                        (cdr table)))))
  'ok)

(define (make-record key value)
  (cons key value))
(define record car)
(define (add-new-entry! empty-node key value)
  (display (list "add-new-entry!" empty-node key value))
  (set-cdr! empty-node (make-tree (make-record key value) '() '())))

(define (set-left-branch! tree entry) (set-car! (cdr tree) entry))
(define (set-right-branch! tree entry) (set-cdr! (cddr tree) entry)) 

(define (adjoin-set x set)
  (let ((new-key (car x)))
    (cond ((null? set) (make-tree-node x))
          ((eq? new-key (caar set)) (car set))
          ((< new-key (entry set))
           (set-left-branch! set (adjoin-set x (left-branch set))))
          ((> new-key (entry set))
           (set-right-branch! set (adjoin-set x (right-branch set)))))))

(define (insert-tree! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (begin
          (set-cdr! table (make-tree (cons key value) '() '()))
          (display table))))
  'ok)

(define t1 (list '*table*))
t1
(insert-tree! 42 'mom t1)
t1

(insert-tree! 42 'dad t1)
(define d1 (cdr t1))