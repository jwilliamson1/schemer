#lang sicp
(define make-table '(*table*))
(define (atom? exp) (not (pair? exp)))
 (define (make-entry key value) (cons key value))
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



(define (lookup keys table)
  (let ((record (assoc keys (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc keys records)
  (define (handle-matches keys record)
    (cond ((null? keys) record)
          ((pair? (cdr record)) (assoc keys (cdr record)))
          (else #f)))
  (display (list "assoc params" "KEYS:" keys "RECORDS:" records))(newline)
  (cond ((or (null? records) (null? (car records))) false)
        ((equal? (car keys) (caar records))
         (handle-matches (cdr keys) (car records)))
        ((< (car keys) (caar records))
         (assoc keys (left-branch records)))        
        (else (assoc keys (right-branch records)))))

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
(define (set-right-branch! tree entry) (set-car! (cddr tree) entry))

(define (nest-trees-for-remaining-keys keys value)
  (display (list "null-set-handler params" "keys:" keys "value:" value))
  (newline)
  (if (null? keys) value
      (make-tree-node (make-entry (car keys) (nest-trees-for-remaining-keys (cdr keys) value)))))
;(keys) -> atom b-> atom b|table a b-> atom b|table a b 
(define (match-handler keys value entry)
  (display (list "match-handler params" "keys:" keys "entry:" entry))
  (newline)
  (cond ((null? keys) value)
        ((pair? entry) (adjoin-set! keys value entry))
        (else (nest-trees-for-remaining-keys keys value))))

(define (adjoin-set! keys value set)
  (display (list "adjoin-set:" set))
  (newline)
  (let ((new-key (car keys)))
    (cond ((null? set) (nest-trees-for-remaining-keys keys value))
          ((eq? new-key (caar set))
           (set-cdr! (car set) (match-handler (cdr keys) value (cdar set)))
           set)
          ((< new-key (entry set))
           (set-left-branch! set (adjoin-set! keys value (left-branch set)))
           set)
          ((> new-key (entry set))
           (set-right-branch! set (adjoin-set! keys value (right-branch set)))
           set))))

(define (insert-tree! keys value table)  
  (set-cdr! table (adjoin-set! keys value (cdr table)))
  (display table)
  'ok)

(define t1 (list '*table*))
(define d1 (cdr t1))
t1
;(set-cdr! t1 (make-tree-node (make-entry 42 'wet)))
t1
(insert-tree! '(42) 'mom t1)
t1
;overwrite root test
(insert-tree! '(42) 'dad t1)
(insert-tree! '(22) 'son t1)
(insert-tree! '(32) 'niece t1)
(insert-tree! '(12) 'nephew t1)
(insert-tree! '(12) 'cousin t1)
(insert-tree! '(62) 'mom t1)
(insert-tree! '(72) 'opa t1)
(insert-tree! '(82) 'gram t1)
(insert-tree! '(82) 'girondin t1)

t1
(define t2 make-table)
t2

(insert-tree! '(12 -5 50/100) 'dani t2)
(insert-tree! '(12 -5 5/8) 'cam t2)
(insert-tree! '(12 -5 1/4) 'casey t2)
(insert-tree! '(12 -3 40/100) 'dad t2)
(insert-tree! '(12 -3 40/100 .333333) 'dad t2)
(insert-tree! '(12 -3 40/100 .333333) 'dada t2)
(insert-tree! '(13) 'motorsport t2)
(insert-tree! '(11) 'motorville t2)
(insert-tree! '(11 2/3) 'dank t2)
(lookup '(11 2/3) t2)
(lookup '(12 -3) t2)
(lookup '(12 -3 40/100 .333333) t2)