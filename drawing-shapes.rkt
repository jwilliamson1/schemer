;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname drawing-shapes) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))

(define-struct circle(center radius color))

;; DATA EXAMPLES
;(make-circle (make-posn 1 1) 100 'red)
;(make-circle (make-posn 10 10) 40 'blue)


(define (draw-a-circle c)
  (draw-circle (circle-center c)
               (circle-radius c)
               (circle-color c)))


(define (clear-a-circle c)
  (clear-circle (circle-center c)
               (circle-radius c)
               (circle-color c)))

(define (in-circle? c p)
  (<= (sqrt (+ (sqr (- (posn-x (circle-center c))
                       (posn-x p)))
               (sqr (- (posn-y (circle-center c))
                       (posn-y p)))))
      (circle-radius c)))

(in-circle? (make-circle (make-posn 6 5) 1 'blue) (make-posn 6 5)) "should be" true
;(in-circle? (make-circle (make-posn 6 5) 1 'green) (make-posn 5.5 5)) "should be" true
;(in-circle? (make-circle (make-posn 6 5) 1 'yellow) (make-posn 1 5)) "should be" false

(define (translate-circle circle delta)
  (make-circle
   (make-posn (+ delta(posn-x(circle-center circle)))
              (posn-y(circle-center circle)))
              (circle-radius circle)
              (circle-color circle)))
   

(start 300 300)
(draw-a-circle
 (translate-circle
  (make-circle(make-posn 1 1) 100 'red)100))

(draw-a-circle(make-circle(make-posn 10 10) 40 'blue))
(clear-a-circle(make-circle(make-posn 10 10) 40 'blue))

(define (draw-and-clear-circle a-circle wait-time)
  (and(draw-a-circle a-circle)
      (sleep-for-a-while wait-time)
      (clear-a-circle a-circle)))

(define test-circle (make-circle(make-posn 1 1) 100 'green))
(draw-and-clear-circle test-circle 1)

(define (move-circle delta a-circle)
  (cond
    [(draw-and-clear-circle a-circle 1/2) (translate-circle a-circle delta)]
    [else a-circle]))

(draw-a-circle 
  (move-circle 10
    (move-circle 10
      (move-circle 10
	(move-circle 10 test-circle)))))

;; A rectangle is a structure:
;;   (make-rectangle P W H)
;; where P is a posn, W is a number and H is a number.
(define-struct rectangle (nw-corner width height color))

;; DATA EXAMPLES
(define example-rectangle1 (make-rectangle (make-posn 20 20) 260 260 'red))
(define example-rectangle2 (make-rectangle (make-posn 60 60) 180 180 'blue))

#|
;; Template
(define (fun-for-rectangle a-rectangle)
  ... (rectangle-nw-corner a-rectangle) ...
  ... (rectangle-width a-rectangle) ...
  ... (rectangle-height a-rectangle) ...
  ... (rectangle-color a-rectangle) ...)
|#

; -------------------------------------------------------------------------

;; draw-a-rectangle : rectangle -> true
;; to draw a-rect
(define (draw-a-rectangle a-rectangle)
  (draw-solid-rect
   (rectangle-nw-corner a-rectangle)
   (rectangle-width a-rectangle)
   (rectangle-height a-rectangle)
   (rectangle-color a-rectangle)))

;; EXAMPLES
(start 300 300)
(draw-a-rectangle example-rectangle1)
(draw-a-rectangle example-rectangle2)

; -------------------------------------------------------------------------

;; in-rectangle? : rectangle posn -> boolean
;; to determine if a-posn is in a-rectangle, or not
(define (in-rectangle? a-rectangle a-posn)
  (and (<= (posn-x (rectangle-nw-corner a-rectangle))
           (posn-x a-posn)
           (+ (posn-x (rectangle-nw-corner a-rectangle))
              (rectangle-width a-rectangle)))
       (<= (posn-y (rectangle-nw-corner a-rectangle))
           (posn-y a-posn)
           (+ (posn-y (rectangle-nw-corner a-rectangle))
              (rectangle-height a-rectangle)))))

;; EXAMPLES AS TESTS
(in-rectangle? example-rectangle1 (make-posn 0 0)) "should be" false
(in-rectangle? example-rectangle1 (make-posn 25 0)) "should be" false
(in-rectangle? example-rectangle1 (make-posn 0 25)) "should be" false
(in-rectangle? example-rectangle1 (make-posn 25 25)) "should be" true

; -------------------------------------------------------------------------

;; translate-rectangle : rectangle number -> rectangle
;; to translate a-rectangle horizontally by x pixels 
(define (translate-rectangle a-rectangle x)
  (make-rectangle (make-posn
                   (+ x (posn-x (rectangle-nw-corner a-rectangle)))
                   (posn-y (rectangle-nw-corner a-rectangle)))
                  (rectangle-width a-rectangle)
                  (rectangle-height a-rectangle)
                  (rectangle-color a-rectangle)))

;; EXAMPLES AS TESTS
(translate-rectangle example-rectangle1 30)
"should be"
(make-rectangle (make-posn 50 20) 260 260 'red)

; -------------------------------------------------------------------------

;; clear-a-rectangle : rectangle -> true
;; to erase a rectangle
(define (clear-a-rectangle a-rectangle)
  (clear-solid-rect 
   (rectangle-nw-corner a-rectangle)
   (rectangle-width a-rectangle)
   (rectangle-height a-rectangle)))

;; EXAMPLES
(start 300 300)
(draw-a-rectangle example-rectangle1)
(draw-a-rectangle example-rectangle2)
(clear-a-rectangle example-rectangle1)
(clear-a-rectangle example-rectangle2)