#lang racket/gui
(require graphics/graphics)
(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))
 
(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))



(define (make-vect x y)
  (cons x y))
 
(define (xcor-vect v)
  (car v))
 
(define (ycor-vect v)
  (cdr v))
 
(define (add-vect v1 v2)
  (make-vect
   (+ (xcor-vect v1) (xcor-vect v2))
   (+ (ycor-vect v1) (ycor-vect v2))))
 
(define (sub-vect v1 v2)
  (make-vect
   (- (xcor-vect v1) (xcor-vect v2))
   (- (ycor-vect v1) (ycor-vect v2))))
 
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(define make-segment cons) 
 (define start-segment car) 
 (define end-segment cdr)
 
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
 
(define (origin-frame frame)
  (car frame))
 
(define (edge1-frame frame)
  (cadr frame))
 
(define (edge2-frame frame)
  (caddr frame))
 
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define (drawline v1 v2)
  (line(make-posn (xcor-vect v1)(ycor-vect v1))
       (make-posn (xcor-vect v2)(ycor-vect v2))))
 
;(define (segments->painter segment-list)
;  (lambda (frame)
;    (for-each
;     (lambda (segment)
;       (let ((start-coord-map ((frame-coord-map frame) (start-segment segment)))
;             (end-coord-map ((frame-coord-map frame) (end-segment segment))))
;       (line
;        (make-posn (xcor-vect start-coord-map) (ycor-vect start-coord-map))
;        (make-posn (xcor-vect end-coord-map) (ycor-vect end-coord-map)))))
;     segment-list)))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (drawline
        ((frame-coord-map frame) 
         (start-segment segment))
        ((frame-coord-map frame) 
         (end-segment segment))))
     segment-list)))

(define x-painter
  (segments->painter
   (list
    (make-segment (make-vect 0 0)
                  (make-vect 1 1))
    (make-segment (make-vect 0 1)
                  (make-vect 1 0)))))
 
(define unit-frame (make-frame (make-vect 0 500) (make-vect 500 0) (make-vect 0 -500)))
;(x-painter unit-frame)

(define (transform-painter 
         painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin
                  (sub-vect (m corner1) 
                            new-origin)
                  (sub-vect (m corner2)
                            new-origin)))))))

(define wave 
   (segments->painter (list 
                       (make-segment (make-vect .25 0) (make-vect .35 .5)) 
                       (make-segment (make-vect .35 .5) (make-vect .3 .6)) 
                       (make-segment (make-vect .3 .6) (make-vect .15 .4)) 
                       (make-segment (make-vect .15 .4) (make-vect 0 .65)) 
                       (make-segment (make-vect 0 .65) (make-vect 0 .85)) 
                       (make-segment (make-vect 0 .85) (make-vect .15 .6)) 
                       (make-segment (make-vect .15 .6) (make-vect .3 .65)) 
                       (make-segment (make-vect .3 .65) (make-vect .4 .65)) 
                       (make-segment (make-vect .4 .65) (make-vect .35 .85)) 
                       (make-segment (make-vect .35 .85) (make-vect .4 1)) 
                       (make-segment (make-vect .4 1) (make-vect .6 1)) 
                       (make-segment (make-vect .6 1) (make-vect .65 .85)) 
                       (make-segment (make-vect .65 .85) (make-vect .6 .65)) 
                       (make-segment (make-vect .6 .65) (make-vect .75 .65)) 
                       (make-segment (make-vect .75 .65) (make-vect 1 .35)) 
                       (make-segment (make-vect 1 .35) (make-vect 1 .15)) 
                       (make-segment (make-vect 1 .15) (make-vect .6 .45)) 
                       (make-segment (make-vect .6 .45) (make-vect .75 0)) 
                       (make-segment (make-vect .75 0) (make-vect .6 0)) 
                       (make-segment (make-vect .6 0) (make-vect .5 .3)) 
                       (make-segment (make-vect .5 .3) (make-vect .4 0)) 
                       (make-segment (make-vect .4 0) (make-vect .25 0))
                       ;smile
                       (make-segment (make-vect .48 .75)(make-vect .52 .75))
                       (make-segment (make-vect .45 .78)(make-vect .48 .75))
                       (make-segment (make-vect .55 .78)(make-vect .52 .75))
                       ))) 
 ;George! 
;(wave unit-frame)

(define (flip-vert painter)
  (transform-painter 
   painter
   (make-vect 0.0 1.0)   ; new origin
   (make-vect 1.0 1.0)   ; new end of edge1
   (make-vect 0.0 0.0))) ; new end of edge2

(define (flip-horiz painter)
  (transform-painter
   painter
   (make-vect 1.0 0.0); new origin
   (make-vect 0.0 0.0)
   (make-vect 1.0 1.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate180 painter)
  (flip-vert painter))

(define (rotate270 painter)
  (flip-horiz (rotate90 painter)))
                     

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

;((squash-inwards(shrink-to-upper-right(flip-vert( rotate90 wave))))unit-frame)
;(wave unit-frame)
;((rotate180(rotate180 wave))unit-frame)

;((rotate180(rotate180 wave))unit-frame)
;((rotate90 wave)unit-frame)
;((rotate270 wave)unit-frame)

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left  (transform-painter 
                        painter1
                        (make-vect 0.0 0.0)
                        split-point
                        (make-vect 0.0 1.0)))
          (paint-right (transform-painter
                        painter2
                        split-point
                        (make-vect 1.0 0.0)
                        (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let  ((paint-top (transform-painter
                       painter1
                       split-point
                       
                       (make-vect 1.0 0.5)
                       (make-vect 0.0 1.0)))
           (paint-bottom (transform-painter
                          painter2
                          (make-vect 0.0 0.0)
                          (make-vect 1.0 0.0)
                          split-point
                          )))
    (lambda(frame)
      (paint-top frame)
      (paint-bottom frame)))))

(define (below-rot painter1 painter2)
  (rotate90 (beside (rotate270 painter2)(rotate270 painter1))))

;((below-rot wave wave)unit-frame)
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
;((below wave wave)unit-frame)
(define right-split (split beside below))
(define up-split (split below beside))

(define (repeat-painter transform painter n)
  (cond ((< n 1) painter)
        ((= n 1)(transform painter painter))
        (else(transform (repeat-painter transform painter (- n 1))
                        (repeat-painter transform painter (- n 1))))))

;((repeat-painter beside wave 2)unit-frame)

(define (corner-split painter n m)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter 
                                (- n 1))))
        (let ((top-left (repeat-painter beside up m))
              (bottom-right (repeat-painter below right m))
              (corner (corner-split painter 
                                    (- n 1)(- m 1))))
          (beside (below painter top-left)
                  (below bottom-right 
                         corner))))))                                       
                                         
 ((corner-split wave 3 3) unit-frame)

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) 
                       (tr painter)))
          (bottom (beside (bl painter) 
                          (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 
         (square-of-four flip-horiz 
                         identity
                         rotate180 
                         flip-vert)))
    (combine4 (corner-split painter n))))