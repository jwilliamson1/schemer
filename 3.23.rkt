#lang sicp

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) 
  (set-car! queue item))
(define (set-rear-ptr! queue item) 
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (rear-queue queue)
  (if (empty-queue? queue)
      (error "REAR called with an empty queue" queue)
      (car (rear-ptr queue))))     

(define (make-node item next prev)
  (list (cons 'item item)
        (cons 'next next)
        (cons 'prev prev)))

(define qn1 (make-node 'a-thing 'next-ptr 'prev-ptr))

(define (queue-node-item queue)(cdr (car queue)))
(define (queue-node-next queue)(cdr (cadr queue)))
(define (queue-node-prev queue)(cdr (caddr queue)))

(queue-node-item qn1)
(queue-node-next qn1)
(queue-node-prev qn1)

(define (insert-rear! queue item)
  (let ((new-pair (make-node item '() '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           ;sets the pointer of the last pair to the new pair
           (set-cdr! (rear-ptr queue)
                          new-pair)
           ;sets the rear pointer
                (set-rear-ptr! queue new-pair)
                queue))))

(define (insert-front! queue item)
  (cond ((empty-queue? queue)
         (let ((new-pair (cons item '())))
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)))
        (else
         (let ((new-front (cons item (front-ptr queue))))
           (set-front-ptr! queue new-front)))))

(define (delete-front! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue"))
        (else (set-front-ptr!
               queue
               (cdr (front-ptr queue)))
              queue)))

(define (delete-rear! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue"))
        (else (set-rear-ptr!
               queue
               ))))

(define (print-queue queue)
  (begin (display (front-ptr queue))
         (newline)))

(define q1 (make-queue))
;(rear-queue A)
(insert-rear! q1 'a)
(rear-queue q1)
(insert-rear! q1 'b)
(insert-front! q1 'c)
(rear-queue q1)
(print-queue q1)
(print-queue (delete-front! q1))
(rear-queue q1)