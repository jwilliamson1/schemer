#lang scheme
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

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
;(paint (rght-split einstein 5))
;(paint (up-split einstein 5)) 
      

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

;(paint(square-limit einstein 5))

(define (make-vect x y)
   (cons x y))

(define (xcor-vect v)
   (car v))

(define (ycor-vect v)
   (cdr v))


(define a-vect(make-vect 3 2))

(xcor-vect a-vect)

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

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define frame1 (make-frame (make-vect 0 0)(make-vect 0 1)(make-vect 1 0)))

(define (origin-frame frame)
  (car frame))

(origin-frame frame1)

(define (edge1-frame frame)
  (cadr frame))

(edge1-frame frame1)

(define (edge2-frame frame)
  (caddr frame))

(edge2-frame frame1)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect 
      (scale-vect (xcor-vect v)
                  (edge1-frame frame))
      (scale-vect (ycor-vect v)
                  (edge2-frame frame))))))

;(define (segments->painter segment-list)
;  (lambda (frame)
;    (for-each
;     (lambda (segment)
;       (draw-line
;        ((frame-coord-map frame) 
;         (start-segment segment))
;        ((frame-coord-map frame) 
;         (end-segment segment))))
;     segment-list)))

 ;; heh 
 (define make-segment cons) 
 (define start-segment car) 
 (define end-segment cdr)

(define outline-segments (list (make-segment (make-vect .02 .02)
                                             (make-vect .02 .98))
                               (make-segment (make-vect .02 .98)
                                             (make-vect .98 .98))
                               (make-segment (make-vect .98 .02)
                                             (make-vect .98 .98))
                               (make-segment (make-vect .02 .02)
                                             (make-vect .98 .02))))

(define x-segments (list(make-segment (make-vect 0 0)
                                      (make-vect 1 1))
                        (make-segment (make-vect 0 1)
                                      (make-vect 1 0))))


(start-segment x-segments)
(end-segment x-segments)


(paint (segments->painter outline-segments))
(paint (segments->painter x-segments))
