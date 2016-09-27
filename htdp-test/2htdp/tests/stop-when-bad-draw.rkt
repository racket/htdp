;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname stop-when-bad-draw) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define (main x)
  (big-bang x
            [to-draw draw-circ]
            [on-tick shrink-circ]
            [stop-when stop-circ?]))

(define (draw-circ x) (circle x 'solid" 'red"))
(define (shrink-circ x) (- x 5))
(define (stop-circ? x) (<= x 0))

(check-expect (main 2) -3)