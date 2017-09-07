#lang racket

(define num-doors 3)

(define (num-to-list n)
  (define (to-list-rec c n)
    (if (> c n)
        (quote())
        (cons c (to-list-rec (+ c 1) n))))
  (to-list-rec 1 n))

;this is a test. all comments are function tests
;(num-to-list 3)

(define (remove-door n list)
  (cond
    ((null? list)(quote()))
    ((= (car list) n)(remove-door n (cdr list)))
    (else(cons (car list)(remove-door n (cdr list))))))


;(pick 1 '(alpha delta dog))
;(pick 2 '(alpha delta dog))
;(pick 3 '(alpha delta dog))

(define (a-trial)
  
  (define prize-door
    (+ 1 (random num-doors)))
  
  (define first-pick
    (+ 1 (random num-doors))) 
  (newline)
  (newline)
  (display "prize door: ")
  (display prize-door)
  
  ;to remove unnecessary trials where 'player' selects correct door or a door with a prize is revealed remove these from initial list
  (define doors
    (num-to-list num-doors))
  (newline)
  (display "initial pick: ")(display first-pick)
  ;get a wonk, a door that isn't the prize door and isn't the initial door. There will always be one more door that is a wonk
  ;regardless of whether initial pick is correct or not. 
  (define wonk (car(remove-door first-pick(remove-door prize-door doors))))
  
  (newline)
  (display "wonk: ")
  (display wonk)

  (define remaining-doors (remove-door wonk doors))  
  
  (newline)
  (display "remaining doors: ")
  (display remaining-doors)
  ;switch doors; this is where most people assume there is a 50/50 chance
  (define second-pick (car(remove-door first-pick remaining-doors)))
  (newline)
  (display "second pick: ")
  (display second-pick)
  
  (if(= prize-door second-pick)
     true
     false)
)

(define (monty-hall n)
  (define total-trials n)
  (define (monty-hall-iter n right)
    (cond
      ((= n 0)(newline)(newline)(display "ratio correct: ")(display (/ right total-trials)))
      ((a-trial)(newline)(display "correct")(monty-hall-iter (- n 1) (+ 1 right)))
      (else (newline)(display "incorrect")(monty-hall-iter (- n 1) right))))
  (monty-hall-iter n 0))

;run simulation 1000 times
(monty-hall 1000)

;(remove-door 1 (num-to-list 3))
;(remove-door 2 (num-to-list 3))
;(remove-door 3 (num-to-list 3))
                      