
#lang sicp
(define (atom? exp) (not (pair? exp)))
 (define (make-entry key value) (cons key value))
(define (entry tree) (caar tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (make-tree-node x) (make-tree x '() '()))

(define tree? pair?)

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

(define (make-record key value)
  (cons key value))
(define record car)
(define (add-new-entry! empty-node key value)
  (display (list "add-new-entry!" empty-node key value))
  (set-cdr! empty-node (make-tree (make-record key value) '() '())))

(define (set-left-branch! tree entry) (set-car! (cdr tree) entry))
(define (set-right-branch! tree entry) (set-car! (cddr tree) entry)) 

(define (adjoin-set! set keys value)
  (define (insert-tree! table value . keys)  
    (set-cdr! table (adjoin-set! (cdr table) keys value))
    ;(display table)
    table)
  (let ((key (car keys)))

    (cond  ((null? (cdr keys)) (make-tree-node (make-entry key value)))
           ((or (null? set)(eq? key (caar set)))
            (if (tree? (cdar set))
                (insert-tree! (cdar set) value (cdr keys))
                (insert-tree! (list '*table*) value (cdr keys))))

          ((< key (entry set))
           (set-left-branch! set (adjoin-set! (left-branch set) keys value ))
           set)
          ((> key (entry set))
           (set-right-branch! set (adjoin-set! (right-branch set) keys value))
           set))))

(define (insert-tree! table value . keys)  
  (set-cdr! table (adjoin-set! (cdr table) keys value))
  (display table)
  'ok)

(define t1 (list '*table*))
;(define d1 (cdr t1))
t1
;(set-cdr! t1 (make-tree-node (make-entry 42 'wet)))
;t1
;(insert-tree! t1 'mom 42)
t1
;overwrite root test

(insert-tree! t1 'cousin  12)
(insert-tree! t1 'cousina  9 3)
;(insert-tree! t1 'cousin-twice-removed 12 5) ;((12 . (*table (5 . cousin))
;(insert-tree! t1 'cousin-thrice-removed 12 5 3) ;((12 . (*table (5 . cousin))
;(insert-tree! 'mom t1 62)
;(insert-tree! 'opa t1 72)
;(insert-tree! 'gram t1 82)
;(insert-tree! 'girondin t1 82)
t1

(lookup '82 t1)
(lookup '42 t1)
(lookup '5 t1)

(insert-tree! (cons 5 'mason) 'washington  12)