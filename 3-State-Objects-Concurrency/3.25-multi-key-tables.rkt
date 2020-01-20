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

(define t3 (list '*table*))
(insert! 'a 1 t3)
(lookup 'a t3)

(define (make-two-key-table same-key?)
  (define (assoc key records)
    (cond ((null? records) false)
          ((same-key? key (caar records)) 
           (car records))
          (else (assoc key (cdr records)))))
  (let ((local-table (list '*table*))) ;table shape ('table ())
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
                  (set-cdr! record value) ;record found
                  (set-cdr! ;create new record
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

(put '20 '10 *)
(get '15 '15)
(get '14 '15)
(get '14 '16)

(define (insert-test! keys val table)
  (define backbone cons)
  (define (insert-new keys val)
    (if (null? (cdr keys))
        (cons (car keys) val)
         (cons (car keys) (backbone (insert-new (cdr keys) val) '()))))
  (define (search keys val table)
    (let ((entry (assoc (car keys) (cdr table))))
      (if (null? (cdr keys)) 
          (if entry   
              (set-cdr! entry val)
              (set-cdr! table
                        (backbone (cons (car keys) val) 
                              (cdr table))))        
          (if entry
              (search (cdr keys) val entry)              
              (set-cdr! table (backbone(insert-new keys val)(cdr table)))))))
  (search keys val table))




(define (insert-more! keys val table)
  (define backbone cons)
  (define no not)
  (define (is-record? entry) (and (pair? entry) (not (pair? (cdr entry))))) 
  (define (update entry value) (set-cdr! entry value))
  (define (insert-new keys val table)
    (if (null? keys)
        val
        (backbone (cons (car keys) (insert-new (cdr keys) val '())) table)))
  (define (search keys val table)
    (let ((entry (assoc (car keys) (cdr table))))
      (if (no entry)
          (set-cdr! table
                    (insert-new keys val (cdr table)))
          (if (is-record? entry)
              (set-cdr! entry
                        (insert-new (cdr keys) val '()))
              (search (cdr keys) val entry)))))
  (search keys val table))

(define (lookup-new keys table)
  (display keys)(newline)
  (if (null? keys) (cdr table)      
      (let ((entry (assoc (car keys) (cdr table))))
        (if entry (lookup-new (cdr keys) entry)
            (display "no entry")))))

(define (make-multi-key-table)
  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (define (iter keys table)     
        (if (null? keys) (cdr table)      
            (let ((entry (assoc (car keys) (cdr table))))
              (if entry (iter (cdr keys) entry)
                  (display "no entry")))))
      (iter keys local-table))
    (define (insert! keys val)      
        (define backbone cons)
        (define no not)
        (define (is-record? entry) (and (pair? entry) (not (pair? (cdr entry))))) 
        (define (update entry value) (set-cdr! entry value))
        (define (insert-new keys val table)
          (if (null? keys)
              val
              (backbone (cons (car keys) (insert-new (cdr keys) val '())) table)))
        (define (iter keys val table)
          (let ((entry (assoc (car keys) (cdr table))))
            (if (no entry)
                (set-cdr! table
                          (insert-new keys val (cdr table)))
                (if (is-record? entry)
                    (set-cdr! entry
                              (insert-new (cdr keys) val '()))
                    (iter (cdr keys) val entry)))))
        (iter keys val local-table))         
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: Table" m))))
    dispatch))

(define t2 (list '*table*))
(insert-more! (list 'a) 1 t2)
(insert-more! (list 'b) 1 t2)
(insert-more! (list 'a) 2 t2)
(insert-more! (list 'a 'letters) 'z t2)
t2
(lookup-new (list 'b) t2)
(lookup-new (list 'a 'letters) t2)

(define t1 (list '*table*))
t1
"***********"
(insert-more! (list 'letters 'a 'more) 'wordz t1)
t1
"***********"

(insert-more! (list 'math '+ 'less) 123 t1)
t1
"***********"
(insert-more! (list 'math '+ 'less) 123 t1)
(insert-more! (list 'math '+ 'less) 456 t1)
t1
(define mt1 (make-multi-key-table))
(define getm (mt1 'lookup-proc))
(define putm (mt1 'insert-proc!))

(getm (list 'a))
(putm (list 'a) 'some)
(getm (list 'a))
(putm (list 'letters 'a) 'some)
(getm (list 'a))
(putm (list 'math 'ints) '4)
(getm (list 'math 'ints))