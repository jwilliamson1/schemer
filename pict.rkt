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
  (lambda(ptr i)
    (split-iter ptr i)
    )
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

(paint(square-limit einstein 5))