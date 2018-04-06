#lang racket
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch 
                (car bits) 
                current-branch)))
          (if (leaf? next-branch)
              (cons 
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) 
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: 
               CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree 
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree 
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

(define sample-tree-2
  (make-code-tree
   (make-code-tree
    (make-code-tree
     (make-leaf 'C 1)
     (make-leaf 'D 1))
    (make-leaf 'B 2))
   (make-leaf 'A 4)))

(define sample-tree-3
  (make-code-tree
   (make-code-tree
    (make-leaf 'A 2)
    (make-leaf 'E 2))
   (make-code-tree
    (make-leaf 'I 2)
    (make-leaf 'O 2))))
    

sample-tree-2

sample-tree

sample-tree-3

(define sample-message 
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

  (define (contains-symbol? branch symbol)
    (if (leaf? branch)
        (equal? (symbol-leaf branch) symbol)        
        #f))

(define (encode-symbol-v1 symbol tree)
  (define (copy-to-bits symbol tree result-list)
    (cond ((null? tree) '())
          ((leaf? tree) (if (equal? (symbol-leaf tree) symbol)
                            result-list
                            '()))
          (else (append(copy-to-bits symbol (left-branch tree) (append result-list '(0)))
                       (copy-to-bits symbol (right-branch tree) (append result-list '(1)))))))
  (copy-to-bits symbol tree '()))

(define (encode-symbol-v2 symbol tree)
  (define (copy-to-bits symbol tree result-list next)
    (cond ((null? tree) '(RIP)) ; never gets here
          ((leaf? tree) (if (equal? (symbol-leaf tree) symbol)
                            result-list
                            next))
          (else (copy-to-bits 
                              symbol
                              (left-branch tree)
                              (cons result-list '(0))                              
                              (copy-to-bits 
                                           symbol
                                           (right-branch tree)
                                           (cons result-list '(1))
                                           null)
                              )
                )))
  (copy-to-bits  symbol tree '9 null))

(define (encode-symbol symbol tree)
  (define (copy-to-bits tree)
    (cond ((contains-symbol? (left-branch tree) symbol) '(0))
          ((contains-symbol? (right-branch tree) symbol) '(1))
          ((leaf? (left-branch tree))(cons 1(copy-to-bits (right-branch tree))))
          ((leaf? (right-branch tree))(cons 0 (copy-to-bits (left-branch tree))))

          ))
  (copy-to-bits tree))

(define (cons-next first rest)
  (cons first (cons rest)))

(define (encode-symbol-v4 symbol tree)
  (define (copy-to-bits symbol tree result-list next)
    (cond ((null? tree) '())
          ((leaf? tree) (if (equal? (symbol-leaf tree) symbol)
                            result-list
                            next))
          (else (copy-to-bits
                 symbol
                 (left-branch tree)
                 (append result-list '(0))
                 (copy-to-bits symbol (right-branch tree) (append result-list '(1)) next))
                       )))
  (copy-to-bits symbol tree '() '()))

;WRONG cons left  branch on when null
(define (encode-symbol-v5 symbol tree)
  (define (to-bits tree bit)
    (cond
      ((leaf? tree)(if (equal? (symbol-leaf tree) symbol)
                       (cons bit '())
                       '()))
      (else
       (append(to-bits (left-branch tree) '0)
              (to-bits (right-branch tree) '1)))))
(cdr(to-bits tree '())))

;(define (encode-symbol symbol tree)
;  (define copy-to-bits symbol tree result-list)

(contains-symbol? (left-branch sample-tree) 'A)
(contains-symbol? (left-branch sample-tree) 'B)
(contains-symbol? (right-branch sample-tree) 'A)
(contains-symbol? (right-branch sample-tree) 'B)
;(symbol-leaf(left-branch sample-tree))

(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol-v4 (car message)
                         
                      tree)
       (encode (cdr message) tree))))

(encode '(E) sample-tree-3)
(caddr sample-tree)
;need to investigate cadddr
(encode '(C) sample-tree-2)
(define leaf-set (make-leaf-set '((A 8)(B 3)(C 1)(D 1)(E 1)(F 1)(G 1)(H 1))))
(define leaf-set2 (make-leaf-set '((A 10)(B 9)(C 8)(D 7))))
(define leaf-set3 (make-leaf-set '((A 12)(C 10)(E 8)(G 6))))

(make-code-tree (car leaf-set)(cadr leaf-set))
(make-code-tree (caddr leaf-set)(cadddr leaf-set))

;(define (generate-huffman-tree pairs)
 ; (successive-merge 
  ; (make-leaf-set pairs)))

leaf-set

(define (merge-tree pairs)
  (cond
    ((null? (cddr pairs))
     (cons (car pairs)(cdr pairs)))       
    (else (let ((left-pair-weight (+ (weight (car pairs))
                                     (weight (cadr pairs))))
                (right-pair-weight (+ (weight (cadr pairs))
                                      (weight (caddr pairs)))))
            
            (if (> right-pair-weight left-pair-weight)
                (let ((code-tree (make-code-tree (car pairs)(cadr pairs))))
                  (merge-node code-tree (cdr pairs)))
                (cons (car pairs)(merge-tree (cdr pairs))))))))

  
(define (next-is-higher node1-weight node2-weight)
  (> node2-weight node1-weight))

(define (weight-node node)
  (weight node))

(define (merge-node node ordered-set)
  (let ((first (car ordered-set)))
    (cond ((null? (cdr ordered-set))
           (if (< (weight node)(weight first))
               (cons node (cons first '()))
               (cons first (cons node '()))))
          (else           
           (if (or(and (< (weight node)(weight first)))
                  (and (= (weight node)(weight first))))
               (cons node (cons first (cdr ordered-set)))
               (cons first (merge-node node (cdr ordered-set))))))))

(define (successive-merge leaf-set)
  (cond
    ((null? (cddr leaf-set)) (make-code-tree (car leaf-set)(cadr leaf-set)))
        (else (successive-merge (merge-node(make-code-tree (car leaf-set)(cadr leaf-set))(cddr leaf-set))))))

(merge-node (make-leaf 'Z 13) leaf-set3)
;(merge-node(merge-node(make-code-tree (car leaf-set)(cadr leaf-set))(cddr leaf-set)))
;(merge-tree(merge-tree leaf-set2))
(successive-merge leaf-set)
