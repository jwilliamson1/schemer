#lang racket
(define (square x) (* x x))

(define (miller-rabin n) 
   (miller-rabin-test (- n 1) n)) 
  
 (define (miller-rabin-test a n) 
   (cond ((= a 0) true) 
         ; expmod is congruent to 1 modulo n 
         ((= (expmod a (- n 1) n) 1) (miller-rabin-test (- a 1) n)) 
         (else false))) 
  
 (define (expmod base exp m) 
   (cond ((= exp 0) 1) 
         ((even? exp) 
          (let ((x (expmod base (/ exp 2) m))) 
            (if (non-trivial-sqrt? x m) 0 (remainder (square x) m)))) 
         (else 
          (remainder (* base (expmod base (- exp 1) m)) 
                     m)))) 
  
 (define (non-trivial-sqrt? n m) 
   (cond ((= n 1) false) 
         ((= n (- m 1)) false) 
         ; book reads: whose square is equal to 1 modulo n 
         ; however, what was meant is square is congruent 1 modulo n 
         (else (= (remainder (square n) m) 1))))

(define nil '()) 
  
 (define (filter predicate sequence) 
   (cond ((null? sequence) nil) 
         ((predicate (car sequence)) 
          (cons (car sequence)  
                (filter predicate (cdr sequence)))) 
         (else (filter predicate (cdr sequence))))) 
  
 (define (accumulate op initial sequence) 
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (accumulate op initial (cdr sequence))))) 
  
 (define (enumerate-interval low high) 
   (if (> low high) 
       nil 
       (cons low (enumerate-interval (+ low 1) high)))) 
  
  
 (define (flatmap proc seq) 
   (accumulate append nil (map proc seq))) 
  
 ;; Here's the unique-pairs from the chapter: 
 (define (unique-pairs n) 
   (flatmap (lambda (i) 
              (map (lambda (j) (list i j)) 
                   (enumerate-interval 1 (- i 1)))) 
            (enumerate-interval 1 n))) 
  
  
 ;; We need to make triples (i j k).  The following will do: 
  
 (define (unique-triples n) 
   (flatmap (lambda (i) 
              (flatmap (lambda (j) 
                         (map (lambda (k) (list i j k)) 
                              (enumerate-interval 1 (- j 1)))) 
                       (enumerate-interval 1 (- i 1)))) 
            (enumerate-interval 1 n)))

(define the-empty-stream '())

(define (stream-null? stream)
  (null? stream))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define-syntax s-cons
  (syntax-rules ()
    ((s-cons head tail)
     (cons head (delay tail)))))

(define (head stream)
  (car stream))

(define (tail stream)
  (force (cdr stream)))

(define (s-ref s n)
  (if (= n 0)
      (head s)
      (s-ref (tail s) (- n 1))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (s-cons
       low
       (stream-enumerate-interval (+ low 1)
                                  high))))



(define (stream-filter pred stream)
  (cond ((stream-null? stream) 
         the-empty-stream)
        ((pred (head stream))
         (s-cons 
          (head stream)
          (stream-filter 
           pred
           (tail stream))))
        (else (stream-filter 
               pred 
               (tail stream)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin 
        (proc (head s))
        (stream-for-each proc 
                         (tail s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))


(define (show x)
  (display-line x)
  x)

(define (s-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (s-cons
       (apply proc (map head argstreams))
       (apply s-map
              (cons proc 
                    (map tail
                         argstreams))))))

(define (s-add s1 s2) 
  (s-map + s1 s2))

(define ones (s-cons 1 ones))

(define integers (s-cons 1 (s-add integers ones)))

(define s (s-cons 1 (s-add s s)))

(s-ref s 3)

(define (s-mul s1 s2) (s-map * s1 s2))
(define p (s-cons 1 (s-mul p p)))
(tail (tail p))
                                   ;2. promise     1. promise
(define factorial (s-cons 1 (s-mul (tail integers) factorial)))
                                   ;3.promise      2.promise
                                   ;4.promise      6.promise
(displayln "factorial")
(s-ref factorial 0)
(s-ref factorial 1)
(s-ref factorial 2)
(s-ref factorial 3)
(s-ref factorial 4)
;1 3 6 10 15
(define (partial-sums s)
  (s-cons (head s)(s-add (tail s) (partial-sums s))))
(s-ref (partial-sums integers) 4)

(define (scale-stream stream factor)
  (s-map
   (lambda (x) (* x factor))
   stream))

(s-ref (scale-stream integers 2) 3)

(define (display-stream-until n s)     ; n-th value included 
  (if (< n 0) 
      the-empty-stream 
      (begin (display (head s))(newline) 
             (display-stream-until (- n 1) (tail s))))) 

(define (expand num den radix)
  (s-cons
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) 
           den 
           radix)))
; num 1 den 7 radix 10 1.3...
; num 3 den 8 radix 10 3.75
(define as (s-cons 'a as))
(define twos (s-cons 2 twos))
(define (divide-one x)(/ 1 x))
(define (raise b p) (list b '^ p))

(define (integrate-series s)
  (begin
    (displayln (list "head of s " (head s)))
    (displayln (list (head s) "/" (head integers)))
    (s-map / s  integers)))

(define exp-series
  (s-cons 
   1 (integrate-series exp-series)))

(displayln "exp-series")
(display-stream-until 5 exp-series)
;1, 1, 1/2, 1/6
(define cosine-series (s-cons 1 (integrate-series (scale-stream sine-series -1))))

(define sine-series (s-cons 0 (integrate-series cosine-series)))

(displayln "cosine")
(display-stream-until 10 cosine-series)
(newline)
(displayln "sine")
(display-stream-until 10 sine-series)
(newline)

(define (mul-series s1 s2)
  (s-cons (* (head s1) (head s2))
          (s-add (scale-stream (tail s2)(head s1))
                       (mul-series (tail s1) s2))))

 (define circle-series 
     (s-add (mul-series cosine-series cosine-series) 
                  (mul-series sine-series sine-series))) 
  
 ; if you see one 1 followed by 0's, your mul-series is correct. 
 (display-stream-until 5 circle-series)

(define (invert-unit-series series)
  (define inverted-unit-series
    (s-cons 1
            (scale-stream
             (mul-series (tail series)
                         inverted-unit-series)
             -1)))
  inverted-unit-series)
(displayln "invert unit series")
(display-stream-until 5 (invert-unit-series circle-series))
(display-stream-until 5 (invert-unit-series sine-series))
(display-stream-until 7 (invert-unit-series cosine-series))
(display-stream-until 5 (mul-series (invert-unit-series cosine-series) cosine-series))
(display-stream-until 5 (mul-series (invert-unit-series sine-series) sine-series))

(define (div-series s1 s2)
  (if (eq? (head s2) 0) (error "cannot divide by zero")
      (mul-series s1
                  (invert-unit-series s2))))
 (displayln "tangent series")
(display-stream-until 7 (div-series sine-series cosine-series))