;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname guess) (read-case-sensitive #t) (teachpacks ((lib "guess-gui.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "guess-gui.rkt" "teachpack" "htdp") (lib "guess.rkt" "teachpack" "htdp")) #f)))
(define(check-guess guess target)
  (cond [(> guess target) 'too_large]
        [(<  guess target) 'too_small]
        [else 'correct]
        )
  )

(guess-with-gui check-guess)