#lang racket
(define-syntax-rule (swap x y)
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))

;(let ([tmp 5]
;      [other 6])
;  (swap tmp other)
;  (list tmp other))

;(let ([set! 5]
;      [other 6])
;  (swap set! other)
;  (list set! other))

(define-syntax rotate
  (syntax-rules ()
    [(rotate a c ...)
     (shift-to (c ... a)(a c ...))]))

(define-syntax shift-to
  (syntax-rules ()
    [(shift-to (from0 from ...)(to0 to ...))
     (let([tmp from0])
       (set! to from) ...
       (set! to0 tmp))]))



(let ([red 1] [green 2] [blue 3][yellow 4])
  ;(rotate red green)      ; swaps to 2 1
  (rotate red green blue yellow) ; rotates swaps to 1 2 then 3 2 to 1 3 2
  (list red green blue yellow))

(define-syntax val
  (lambda (stx)
    (display stx)
    (syntax-case stx ()
      [val (identifier? (syntax val))(syntax (get-val))])))

(define-values (get-val put-val!)
  (let ([private-val 0])
    (values (lambda () private-val)
            (lambda (v)
              (set! private-val v)))))

(define-syntax val2
  (make-set!-transformer
   (lambda (stx)
     (syntax-case stx (set!)
       [val2 (identifier? (syntax val2))(syntax (get-val))]
       [(set! val2 e)(syntax (put-val! e))]))))

(+ val 3)
val2; 0
(+ val2 3) ;3
(set! val2 10)
val2 ; 10

(define-syntax-rule (define-get/put-id id get put!)
    (define-syntax id
      (make-set!-transformer
       (lambda (stx)
         (syntax-case stx (set!)
           [id (identifier? (syntax id)) (syntax (get))]
           [(set! id e) (syntax (put! e))])))))

(define-get/put-id val3 get-val put-val!)
(set! val3 11)
val3

(define-syntax-rule (define-cbr (id arg ...) body)
  (begin
    (define-syntax id
      (syntax-rules ()
        [(id actual (... ...))
         (do-f (lambda () actual)
               (... ...)
               (lambda (v)
                 (set! actual v))
               (... ...))]))
    (define-for-cbr do-f (arg ...)
      () ; explained below...
      body)))

(define-syntax define-for-cbr
  (syntax-rules ()
    [(define-for-cbr do-f (id0 id ...)
       (gens ...) body)
     (define-for-cbr do-f (id ...)
       (gens ... (id0 get put)) body)]
    [(define-for-cbr do-f ()
       ((id get put) ...) body)
     (define (do-f get ... put ...)
       (define-get/put-id id get put) ...
       body)]))

(define-cbr (f a b)
  (swap a b))
 
(let ([x 1] [y 2])
  (f x y)
  (list x y))

(define-syntax while
  (syntax-rules ()
    ((while condition body ...)(let loop ()
       (if condition
           (begin
             body ...
             (loop))
           #f)))))

(define x 0)

(while (< x 5)
       (set! x (+ x 1))
       (print x))

(define-syntax for
  (syntax-rules (in as)
    ((for element in list body ...)
     (map (lambda (element)
            body ...)
          list))
    ((for element as list body ...)
     (map (lambda (element)
            body ...)
          list))))

(for i in '(0 1 2 3 4) (print i))
(for i as '(0 1 2 3 4) (print i))