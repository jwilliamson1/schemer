#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  ;(displayln tree)
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
    (displayln result-list)
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

;deep-right-tree

;(tree->list-1 deep-right-tree)
;(tree->list-1 right-weight-tree)
;(tree->list-1 left-weight-tree)

;(tree->list-2 deep-right-tree)
;(tree->list-2 right-weight-tree)
;(tree->list-2 left-weight-tree)
;O(2^n)

;(tree->list-2(make-tree 7 (make-tree 3 (make-tree 1 '()'())(make-tree 5 '()'()))'()))
(define c 6)
(define lft-sz(quotient (- c 1) 2))
(define rght-sz(- c (+ lft-sz 1)))
lft-sz
rght-sz
(cons '()'(1 2))

(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))

(define (partial-tree elts n)
  ;(displayln n)
  ;(displayln elts)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size 
             (quotient (- n 1) 2)))
        (let ((left-result 
               (partial-tree 
                elts left-size)))
          (let ((left-tree 
                 (car left-result))
                (non-left-elts 
                 (cdr left-result))
                (right-size 
                 (- n (+ left-size 1))))
            (let ((this-entry 
                   (car non-left-elts))                  
                  (right-result 
                   (partial-tree 
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree 
                     (car right-result))
                    (remaining-elts 
                     (cdr right-result)))
                ;(displayln (cons "remaining elts: " remaining-elts))
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree)
                      remaining-elts)
                )))))))
;Θ(n)
(list->tree '(1 3 5 7 9 11))
;(right-branch(left-branch(car(partial-tree '(1 2 3 4 5 6 7) 7))))
(car(partial-tree '(1) 1))

(define (element-of-set?* x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set?* x (cdr set)))))

(define (intersection-set* set1 set2)
  (cond((or(null? set1)(null? set2))
        '())
       ((element-of-set?* (car set1) set2)
        (cons (car set1)
              (intersection-set* (cdr set1)
                                 set2)))
       (else (intersection-set* (cdr set1)
                                set2))))

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

(define odd-tree (list->tree '(1 3 5 7 9 11)))
(define even-tree (list->tree '(2 4 6 8 10 12)))

(define (balanced-trees-op op tree1 tree2)
  (let ((ordered-list1 (tree->list-2 tree1))
        (ordered-list2 (tree->list-2 tree2)))
        (list->tree
         (op ordered-list1 ordered-list2))))


(define(intersect-btrees tree1 tree2)
  (balanced-trees-op intersection-set*  tree1 tree2))

(define(union-btrees tree1 tree2)
  (balanced-trees-op union-set  tree1 tree2))

(intersect-btrees even-tree odd-tree)
(union-btrees even-tree odd-tree)

(define recordset1 '((1 "blue" 1.9)
                     (2 "grey" 3.45)
                     (3 "rouge" 1)
                     (4 "chartreuse" 4)
                     (5 "vermillion" 8.34)
                     (6 "teal" 0.1)
                     (7 "onyx" 11.3)
                     )
  )

(define (store-ordered-recordset recordset)
  (list->tree recordset))


(define first-col-key car)

(define (lookup given-key set-of-records)
  (define (key record-row)
    (car record-row))
  (cond ((null? set-of-records) false)
        ((equal? given-key 
                 (key (car set-of-records)))
         (car set-of-records))
        (else 
         (lookup given-key 
                 (cdr set-of-records)))))    

;(lookup 2 recordset1)

(define db1(store-ordered-recordset recordset1))

(define (db-lookup given-key set-of-records)
    (define (key record-row)
    (car record-row))
  (cond ((null? set-of-records) false)
        ((= given-key (key(entry set-of-records))) entry set-of-records)
        ((< given-key (key(entry set-of-records)))
         (db-lookup given-key (left-branch set-of-records)))
        (else 
         (db-lookup given-key (right-branch set-of-records)))))

(db-lookup 6 db1)