#lang racket
(require (for-syntax racket/string racket/syntax))
(define-syntax (hyphen-define/ok2 stx)
    (syntax-case stx ()
      [(_ a b (args ...) body0 body ...)
       (with-syntax ([name (format-id stx "~a-~a" #'a #'b)])
         #'(define (name args ...)
             body0 body ...))]))

(hyphen-define/ok2 foo bar () #t)
(foo-bar)

(define-syntax (hyphen-define* stx)
  (syntax-case stx ()
    [(_ (names ...)(args ...) body0 body ...)
     (let* ([names/sym (map syntax-e (syntax->list #'(names ...)))]
            [names/str (map symbol->string names/sym)]
            [name/str (string-join names/str "-")]
            [name/sym (string->symbol name/str)])
       (with-syntax ([name (datum->syntax stx name/sym)])
       #`(define (name args ...)
           body0 body ...)))]))
(hyphen-define* (foo bar baz) (v) (* 2 v))
(foo-bar-baz 50)

(define-syntax (our-struct stx)
    (syntax-case stx ()
      [(_ id (fields ...))
       ; Guard or "fender" expression
       (for-each (lambda(x)
                   (unless (identifier? x)
                     (raise-syntax-error #f "not an identifier" stx x)))
                 (cons #'id (syntax->list #'(fields ...))))
       (with-syntax ([pred-id (format-id stx "~a?" #'id)])
         #`(begin
             ; Define a constructor.
             (define (id fields ...)
               (apply vector (cons 'id  (list fields ...))))
             ; Define a predicate.
             (define (pred-id v)
               (and (vector? v)
                    (eq? (vector-ref v 0) 'id)))
             ; Define an accessor for each field.
             #,@(for/list ([x (syntax->list #'(fields ...))]
                           [n (in-naturals 1)])
                  (with-syntax ([acc-id (format-id stx "~a-~a" #'id x)]
                                [ix n])
                    #`(define (acc-id v)
                        (unless (pred-id v)
                          (error 'acc-id "~a is not a ~a struct" v 'id))
                        (vector-ref v ix))))))]))

(require rackunit)
(our-struct foo (a b))
(define s (foo 1 2))
(check-true (foo? s))
(check-false (foo? 1))
(check-equal? (foo-a s) 1)
(check-equal? (foo-b s) 2)
(check-exn exn:fail?
           (lambda ()(foo-a "furble")))



;This helper function:
(define/contract (hash-refs h ks [def #f])
  ((hash? (listof any/c)) (any/c) . ->* . any)
  (with-handlers ([exn:fail? (const (cond [(procedure? def)(def)]
                                          [else def]))])
    (for/fold ([h h])
              ([k (in-list ks)])
      (hash-ref h k))))



(define-syntax (hash.refs stx)
  (syntax-case stx ()
    ;check for no args at all
    [(_)
     (raise-syntax-error #f "Expected hash.key0[.key1 ...] [default]" stx #`chain)]
    ;If the optional 'default' is missing, use #f.
    [(_ chain)
     #'(hash.refs chain #f)]
    [(_ chain default)
     (unless (identifier? #'chain)
       (raise-syntax-error #f "Expected hash.key0[.key1 ...] [default]" stx #'chain))
     (let* ([chain-str (symbol->string (syntax->datum #'chain))]
            [ids (for/list ([str (in-list (regexp-split #rx"\\." chain-str))])
                            (format-id #'chain "~a" str))])
       ;check that we have at lead hash.key
       (unless (and (>= (length ids) 2)
                    (not (eq? (syntax-e (cadr ids)) '||)))
         (raise-syntax-error #f "Expected hash.key" stx #'chain))
       (with-syntax ([hash-table (car ids)]
                     [keys (cdr ids)])
         #'(hash-refs hash-table 'keys default)))]))

(define js (hasheq 'a (hasheq 'b (hasheq 'c "value"))))
js
 (hash-ref (hash-ref (hash-ref js 'a) 'b) 'c)
(hash-refs js '(a b c))
(hash.refs js.a.b.c)



;(aif #f (displayln it)(void))

(let ([x "outer"])
  (let ([x "inner"])
    (printf "The inner `x' is ~s\n" x))
  (printf "The outer `x' is ~s\n" x))

(define current-foo (make-parameter "some default value"))
(current-foo)

(parameterize ([current-foo "I have a new value, for now"])
    (current-foo))
(current-foo)

(require racket/stxparam)
(define-syntax-parameter it
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "can only be used inside aif")))
(define-syntax-rule (aif condition true-expr false-expr)
  (let ([tmp condition])
    (if tmp
        (syntax-parameterize ([it (make-rename-transformer #'tmp)])
          true-expr)
        false-expr)))

(aif 10 (displayln it)(void))

(aif #f (displayln it)(void))

(let ([it 10])
  it)

(require racket/splicing)

(splicing-let ([x 0])
  
  (define (get-x)
    x))

(get-x)

;(splicing-let ([x 0])
;  (define (inc)
;    (set! x (+ x 1)))
;  (define (dec)
;    (set! x (- x 1)))
;  (define (get)
;    x))

(define-values (inc dec get)
  (let ([x 0])
    (values (lambda () ;inc
              (set! x (+ 1 x)))
            (lambda () ;dec
              (set! x (- 1 x)))
            (lambda () ;get
              x))))

      
(inc)
(dec)
(dec)
(get);-1

(define/contract (misuse s)
  (string? . -> . string?)
  (string-append s " snazzy suffix"))
;User of the function:
(misuse 0)