#lang sicp
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) 
         (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) 
                        (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

(define table-1 (make-table))

(insert! 1 'joe table-1)
(insert! 2 'alia table-1)

(insert! 3 'remy table-1)

(lookup 1 table-1)

(define (make-two-key-table same-key?)
  (define (assoc key records)
    (cond ((null? records) false)
          ((same-key? key (caar records)) 
           (car records))
          (else (assoc key (cdr records)))))
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable 
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record 
                   (assoc key-2 
                          (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable 
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record 
                   (assoc key-2 
                          (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! 
                   subtable
                   (cons (cons key-2 value)
                         (cdr subtable)))))
            (set-cdr! 
             local-table
             (cons (list key-1
                         (cons key-2 value))
                   (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: 
                          TABLE" m))))
    dispatch))

(define (within-tolerance? tolerance)
  (lambda (key1 key2)
    (let ((upper (+ key1 tolerance))
          (lower (- key1 tolerance)))
      (and (>= key2 lower)(<= key2 upper)))))

(define tolerance-test? (within-tolerance? 5))
(tolerance-test? 5 10)
(tolerance-test? 5 11)
(tolerance-test? 5 -1)
(define operation-table (make-two-key-table (within-tolerance? 5)))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 20 10 *)
(get 15 15)
(get 14 15)
(get 14 16)