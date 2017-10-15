#lang racket
;recursive process starts with lowest n, 1, and builds to k as the final condition
(define (cont-frac n d k)
  (define (rcr inc)
    (if (= inc k)
        (/ (n inc)(d inc))    
        (/  (n inc)
            (+ (d inc) (rcr (+ inc 1))))))
  (rcr 1))


;iterative process starts with terminating fraction 1/1 = 1 
(define (cont-frac-iter n d k)
  (define (iter i a)
    (if (= i 0)
        a        
         (iter (- i 1) (/(n i)(+ (d i) a)))))
  (iter (- k 1) (/ (n k)(d k)) ))


(cont-frac(lambda (i) 1.0)
           (lambda (i) 1.0)
           30)

(cont-frac-iter (lambda (i) 1.0)
           (lambda (i) 1.0)
           30)

(cont-frac(lambda (i) 1.0)
          (lambda (i) 
                 (if (= (remainder i 3) 2) 
                     (/ (+ i 1) 1.5) 
                     1.0)) 
               30)

(cont-frac-iter(lambda (i) 1.0)
               (lambda (i) 
                 (if (= (remainder i 3) 2) 
                     (/ (+ i 1) 1.5) 
                     1.0)) 
               30)

(cont-frac(lambda (i)(if(> i 1)(* i i) i))
          (lambda (i)(*(+ (- i 1) i)-1))
               30)

(define tan-n(lambda (x)(lambda (i)(if(> i 1)(* -1 (* x x)) x))))
(define tan-d(lambda (i)(+ (- i 1) i)))

((tan-n 30)1) ;30
((tan-n 30)2) ;900
((tan-n 30)3) ;900

(tan-d 1) ;-1
(tan-d 2) ;-3
(tan-d 3) ;-5

(define (tan-cf x k)
  (cont-frac (tan-n x)
             tan-d
             k))

(define (tan-cf-iter x k)
  (cont-frac-iter (tan-n x)
                  tan-d
                  k))
(tan 30)
(tan-cf 30 1)
(tan-cf-iter 30 1)
