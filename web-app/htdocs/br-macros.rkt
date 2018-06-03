#lang br

(define-macro (report EXPR)
  #'(begin
      (displayln (format "input was ~a" 'EXPR))
      EXPR))

(report (* 4 2 3))
(report (append '(who is this)'(I am immortal)))