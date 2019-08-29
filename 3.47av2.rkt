#lang sicp

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell) (set-car! cell false))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define a-mutex (make-mutex))
(a-mutex 'aquire)
(a-mutex 'aquire)
(display "hi")
(a-mutex 'release)

(define s (make-serializer))
(s (display "test"))

(define (repeat n)
  (if (= n 0)
      'done
      (begin
      (parallel-execute
       (lambda () (withdraw a1 1))
       (lambda () (withdraw a1 2))
       (lambda () (withdraw a1 3))
       (lambda () (withdraw a1 4))
       (lambda () (withdraw a1 1))
       (lambda () (withdraw a1 2))
       (lambda () (withdraw a1 3))
       (lambda () (withdraw a1 4))
       )
      (repeat (- n 1)))
      ))