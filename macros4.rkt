#lang typed/racket
(: misuse (String -> String))
(define (misuse s)
  (string-append s " snazzy suffix"))
(misuse 0)
