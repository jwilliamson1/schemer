#lang racket
 (define *the-table* (make-hash));make THE table 
 (define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value));put 
 (define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f));get

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

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

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: 
              TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: 
              CONTENTS" datum)))

(define (div-A-rec name age salary dept)
  (list name (list age salary dept)))

(define (div-B-rec name age salary dept)
  (list name (list salary age dept)))

(define file-1(attach-tag 'div-a(list
                                  (div-A-rec "Joseph Williamson" 35 "500,000" "research")
                                  (div-A-rec "Chance the rapper" 25 "1,500,000" "youtube sensation")
                                  (div-A-rec "Cardi B" 100 "3,600,000" "trappin"))))

(define file-2 (attach-tag 'div-b (list->tree(list
                                    (div-B-rec "archema" 32 "234,234,234" "rnnnteer")
                                    (div-B-rec "monty pyton" 65 "0" "anarchist")                                                                        
                                    (div-B-rec "zaphod breezlbrox" 15549 "23,000,000,000" "charlatan")))))

                                                
file-2

(define (db-lookup-string given-key set-of-records)
    (define (key record-row)
    (car record-row))
  (cond ((null? set-of-records) false)
        ((string=? given-key (key(entry set-of-records))) (entry set-of-records))
        ((string<? given-key (key(entry set-of-records)))
         (db-lookup-string given-key (left-branch set-of-records)))
        (else 
         (db-lookup-string given-key (right-branch set-of-records)))))

;(db-lookup-string "monty pyton" (cdr file-2))

;division a uses a simple table structure
(define (install-division-A-file-retrieval-system)
 
  (define (get-div-A-record name records)
    (let ((result-list (filter (lambda(x)(string=? name (car x))) records)))
      (if (null? result-list) #f
          (car result-list))))

  (define (get-div-B-record name records)
    (db-lookup-string name records))

  (define (get-record name all-files)    
    ((get 'get-record (type-tag all-files)) name (contents all-files)))

  (put 'get-record 'div-b get-div-B-record)
  (put 'get-record 'div-a get-div-A-record)  
  (put 'get-record 'get-record get-record)
  'done)

(install-division-A-file-retrieval-system)

(define (get-record name records)
  ((get 'get-record
       'get-record) name records))
file-1
file-2

(get-record "Joseph Williamson" file-1)
(get-record "nope" file-1)
(get-record "zaphod breezlbrox" file-2)
(get-record "nuh uh" file-2)