#lang sicp
(define atom?
  (lambda (x)
    (and (not (null? x))
         (not (pair? x)))))

(atom? 'a)
(atom? 99)
(atom? '(1 2 3))
(atom? nil)
(atom? '())

(define apair?
  (lambda (x)
    (cond ((atom? x) #f)
          ((null? x) #f)
          ((null? (cdr x)) #f)
          (else (null? (cdr (cdr x)))))))

(apair? '(a b))
(apair? '(a b c))
(apair? '())
(apair? '(()()))

(define first
  (lambda (x)
    (car x)))

(define second
  (lambda (x)
    (car (cdr x))))

(second '(a b))

(define third
  (lambda (x)
    (car (cdr (cdr x)))))

(third '(a b c))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(build '(aad) '(fda))

(define revpair
  (lambda (pair)
    (build (second pair)(first pair))))

(define revrel
  (lambda (x)
    (cond ((null? x) '())
          (else (cons (revpair (car x))
                      (revrel (cdr x)))))))

(revrel '((8 a)(pumpkin pie)(got sick)))

(define s1 (string #\c#\a#\r))
s1
(string-set! s1 1 #\d)
s1

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(shift '((a b) c))

(shift '((a b)(c d)))

(define align
  (lambda (pora)
    (cond
      (( atom? pora) pora)
      (( apair? (first pora))
       ( align (shift pora)))
      (else ( build (first pora)
                    ( align (second pora)))))))

(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (length* (first pora))
               (length* (second pora)))))))

(length* '())
(length* 'c)
(length* '(a b))
(length* '((a b)(c d)))
(length* '((a b) c))

(align '((a b) c))
(align '((a b)(c d)))
(align '(a (b (c d))))
(align '(a (b c)))