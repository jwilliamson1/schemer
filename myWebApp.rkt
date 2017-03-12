#lang web-server/insta
;; A "hello world" web server
(define (start request)
  (response/xexpr
   '(html
     (head (title "Racket")
           (meta ((charset "UTF-8"))
                   ))
     (body "Hello World")
     (p "This is an example")
     (a ((href "http://www.google.com")) "Past")
     (p "This is " (div ((id "lisp")(class "emph")) "another") " example.")
     (script "var myDiv = document.getElementById('lisp'); myDiv.style.fontSize=24;"))))
