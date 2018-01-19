#lang sicp
(#%require sicp-pict)

;(paint einstein)

(define (rght-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (rght-split painter 
                                  (- n 1))))
        (beside painter 
                (below smaller smaller)))))

(define (upp-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (upp-split painter
                               (- n 1))))
        (below painter
               (beside smaller smaller)))))

(define (split op1 op2)
  (define (split-iter painter n)
    (if (= n 0)
        painter
        (let ((smaller (split-iter painter
                                   (- n 1))))
          (op1 painter
               (op2 smaller smaller)))))
  split-iter  
  )

(define right-split (split beside below))
(define up-split (split below beside))
(paint (rght-split einstein 5))
(paint (up-split einstein 5)) 
      

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter 
                                (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right 
                                   right))
              (corner (corner-split painter 
                                    (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right 
                         corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) 
                        quarter)))
      (below (flip-vert half) half))))

(paint(square-limit einstein 5))

(define (make-vect x y)
  (cons x (cons y nil)))

(define a-vect(make-vect 3 2))

(define (xcor-vect v)
  (car v))

(xcor-vect a-vect)

(define (ycor-vect v)
  (cadr v))

(ycor-vect a-vect)

(define(op-vect op)
  (lambda(v1 v2)
    (make-vect
     (op (xcor-vect v1)(xcor-vect v2))
     (op (ycor-vect v1)(ycor-vect v2)))))

(define add-vect
  (op-vect +))

(define sub-vect
  (op-vect -))

(define (scale-vect s v)
  (make-vect (* s(xcor-vect v))
             (* s(ycor-vect v))))
  

(xcor-vect((op-vect +)a-vect a-vect))

(xcor-vect(add-vect a-vect a-vect))
(ycor-vect(add-vect a-vect a-vect))

(xcor-vect(sub-vect a-vect a-vect))
(ycor-vect(sub-vect a-vect a-vect))

(xcor-vect(scale-vect 10 a-vect))
(ycor-vect(scale-vect 10 a-vect)) 
