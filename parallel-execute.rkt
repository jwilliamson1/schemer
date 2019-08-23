#lang racket
(require (planet dyoo/sicp-concurrency:1:2/sicp-concurrency))

(define (foreach proc . actions)
  (if (null? actions) 'done
      (begin (proc (car actions))
             (foreach (cdr actions)))))
(define (parallel-execute . thunks)
  (define (ensure-end ts)
    (cond ((null? ts) 'done)
          ((memq (thread-state (car ts)) '(terminated dead))
           (begin (thread-join! (car ts)) (ensure-end (cdr ts))))
          (else
           (ensure-end (append (cdr ts) (list (car ts)))))))

    (let ((threads (map make-thread thunks)))
      (for-each thread-start! threads)
      (ensure-end threads)))
  
(parallel-execute (λ () (+ n 1)))