#lang sicp
;these are actually deques but i got lazy
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))

(define (set-front-ptr! deque item) 
  (set-car! deque item))
(define (set-rear-ptr! deque item) 
  (set-cdr! deque item))

(define (empty-deque? deque)
  (null? (front-ptr deque)))

(define (make-deque) (cons '() '()))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (car (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (car (rear-ptr deque))))     

(define (make-node item prev next)
  (list (cons 'item item)
        (cons 'next next)
        (cons 'prev prev)))

(define q0 (make-node 'ground-zero '() '()))
(define qn1 (make-node 'my-thing '() '()))
(define q2 (make-node 'another-thing '() '()))

(define (node-item deque)(cdr (car deque)))
(define (next-node deque)(cdr (cadr deque)))
(define (prev-node deque)(cdr (caddr deque)))

(define (set-next-node! node item)(set-cdr! (cadr node) item))
(define (set-prev-node! node item)(set-cdr! (caddr node) item))

;(set-prev-node! q0 qn1)
;q0
;(set-next-node! q0 q2)
;q0
;(deque-node-item qn1)
;(deque-node-next qn1)
;(deque-node-prev qn1)

(define (insert-rear! deque item)
  (let ((new-node (make-node item (rear-ptr deque) '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-node)
           (set-rear-ptr! deque new-node)
           (print-deque deque))
          (else
           ;sets the pointer of the next node to the new node
           (set-next-node! (rear-ptr deque) new-node)     
           ;sets the rear pointer
           (set-rear-ptr! deque new-node)
           (print-deque deque)))))

(define (insert-front! deque item)
  (let ((new-node (make-node item '() (front-ptr deque))))
  (cond ((empty-deque? deque)
           (set-front-ptr! deque new-node)
           (set-rear-ptr! deque new-node))
        (else
         (set-prev-node! (front-ptr deque) new-node)
         (set-front-ptr! deque new-node)
         (print-deque deque)))))

(define (delete-front! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque"))
        (else (set-front-ptr! deque
                              (next-node (front-ptr deque)))
              (set-prev-node! (front-ptr deque) '())
              (print-deque deque))))

(define (delete-rear! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque"))
        (else (set-rear-ptr! deque (prev-node (rear-ptr deque)))
              (set-next-node! (rear-ptr deque) '())
              (print-deque deque)
               )))

(define (print-deque deque)
  (define (skip-node node)
    (next-node (next-node node))) 
  
  (define (iter node next result)
    (cond ((null? next) (list "next is null: " (reverse (cons (node-item node) result))))
          ((eq? (next-node node) (next-node next)) (list " ***STOPPING: CYCLE DETECTED***: " (reverse (cons (node-item node) result))))
          ((null? (next-node next)) (reverse (cons (node-item (next-node node)) result)))
          (else 
           (iter (next-node node) (skip-node next) (cons (node-item node) result)))))
  
  (let ((first-node (front-ptr deque)))
    (display (iter first-node (next-node first-node) '()))
    (newline)))



(define q1 (make-deque))
;q1
;(rear-ptr q1)
;(rear-deque q1) error
(insert-rear! q1 'a)
(insert-rear! q1 'b)
(insert-rear! q1 'c)
(rear-ptr q1)
;(delete-rear! q1)
(set-next-node! (rear-ptr q1) (front-ptr q1))
(insert-front! q1 'z)

(print-deque q1)
(newline)
(node-item (next-node (next-node (next-node(next-node (next-node (next-node (next-node (front-ptr q1)))))))))
(node-item (next-node (next-node (next-node (next-node(next-node (next-node (next-node (next-node (front-ptr q1))))))))))
;(insert-rear! q1 (front-ptr q1))
;(print-deque q1)
;(print-deque (delete-front! q1))
;(rear-deque q1)