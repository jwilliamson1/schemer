;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname quadratic) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define(how-many a b c)
  (cond[(= 0 a) "degenerate"]
       [(>(* b b)(* 4 a c)) 2]
       [(=(* b b)(* 4 a c)) 1]
       [(<(* b b)(* 4 a c)) 0]
       )
  )
(define(how-many2 a b c)
  (cond[(= 0 a) "degenerate"]
       [(>(discriminant a b c)0) 2]
       [(=(discriminant a b c)0) 1]
       [(<(discriminant a b c)0) 0]
       )
  )
(define(discriminant a b c)
  (-(* b b)(* 4 a c)))