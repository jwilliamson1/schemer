#lang racket
(define-syntax foo
    (lambda (stx)
      (syntax "I am foo")))

foo

(define-syntax (also-foo stx)
    (syntax "I am also foo"))

also-foo

(define-syntax (say-hi stx)
    #'(displayln "hi"))

say-hi

(define-syntax (show-me stx)
    (print stx)
    #'(void))
> (show-me '(+ 1 2))

(define stx #'(if x (list "true") #f))
stx

(define huh #'(if x (list "true") #f))
huh

(syntax-source stx)

(syntax-line stx)

(syntax-column stx)

(syntax-e stx)

(define-syntax (reverse-me stx)
  (datum->syntax stx (reverse (cdr (syntax->datum stx)))))

(reverse-me "backwards" "am" "i" values)

(define (display-and-return x)
    (displayln x))

(define (our-if condition true-expr false-expr)
  (cond [condition true-expr]
        [else false-expr]))
(our-if #t
        (display-and-return "true")
        (display-and-return "false"))
(display "v2")
(newline)
(define-syntax (our-if-v2 stx)
  (define xs (syntax->list stx))
  (datum->syntax stx `(cond[,(cadr xs) ,(caddr xs)]
                       [else ,(cadddr xs)])))

(our-if-v2 #t
             (display-and-return "true")
             (display-and-return "false"))

(require (for-syntax racket/match))
(define-syntax (our-if-using-match-v2 stx)
    (match (syntax->list stx)
      [(list _ condition true-expr false-expr)
       (datum->syntax stx `(cond [,condition ,true-expr]
                                 [else ,false-expr]))]))
(our-if-using-match-v2 #t "true" "false")

(define-syntax (our-if-using-syntax-case stx)
  (syntax-case stx ()
    [(_ condition true-expr false-expr)     
     #'(cond [condition true-expr]
             [else false-expr])]))
(our-if-using-syntax-case #f "right" "wrong")

(define-syntax (hyphen-define/wrong1.2 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (syntax-case (datum->syntax stx
                                 (string->symbol (format "~a-~a" #'a #'b)))
       ()
       [name #'(define (name args ...)
                 body0 body ...)])]))

(hyphen-define/wrong1.2 foo bar () #t)
