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

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
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

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue"))
        (else (set-front-ptr!
               queue
               (cdr (front-ptr queue)))
              queue)))

(define (print-queue queue)
  (begin (display (front-ptr queue))
         (newline)))

(define A (make-queue))
(insert-queue! A 'a)
(insert-queue! A 'b)
(print-queue A)
(print-queue (delete-queue! A))
(print-queue (delete-queue! A))
(print-queue (delete-queue! A))