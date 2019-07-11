#lang sicp
(define (new-table)
  (define (make-entry key value) (cons key value))
  (define (entry tree) (caar tree))
  (define (left-branch tree) (cadr tree))
  (define (right-branch tree) (caddr tree))
  (define (make-tree entry left right)
    (list entry left right))
  (define (make-tree-node x) (make-tree x '() '()))
  
  (define (set-left-branch! tree entry) (set-car! (cdr tree) entry))
  (define (set-right-branch! tree entry) (set-car! (cddr tree) entry))
  
  (let ((table (list '*table*)))
    
    (define (lookup keys)
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
    
    (define (insert-tree! keys value)
      (display (list "INSERT TREE PARAMS:" keys value table))
      (set-cdr! table (adjoin-set! keys value (cdr table)))
      (display table)
      'ok)
    
    (define (print) (display table)(newline))
    (define (dispatch m)
      (cond ((eq? m 'lookup) (lambda (keys)(lookup keys)))
            ((eq? m 'print) print)
            ((eq? m 'insert) (lambda (keys value) (insert-tree! keys value)))
            (else "Invalid command")))
    dispatch))

;PROCEDURAL INTERFACES
(define t4 (new-table))
(define (insert! table keys value)
  ((table 'insert) keys value))
(define (print table)
  ((table 'print)))
(define (lookup table keys)
  ((table 'lookup) keys))

;TESTS
(insert! t4 '(76 -456) 'jesuit)
(insert! t4 '(76 -834) 'chomsky)
(insert! t4 '(76 -1000) 'regime)
(insert! t4 '(50 1/2) 'francoi)
(insert! t4 '(50 1/2 .333) 'twei)
(insert! t4 '(50 1/2 .666) 'cambodia)
(lookup t4 '(50 1/2 .333)) ;twei
(lookup t4 '(76 -456)) ;false because it should have been overwritten
(insert! t4 '(76 -456) 'carmelite)
(print t4);(*table* (76 (-456 . carmelite) ((-834 . chomsky) ((-1000 . regime) () ()) ()) ()) ((50 (1/2 (0.333 . twei) () ((0.666 . cambodia) () ())) () ()) () ()) ())