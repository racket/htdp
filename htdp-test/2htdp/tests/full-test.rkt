#lang htdp/isl+

(require 2htdp/universe)
(require 2htdp/image)

;; {'fullscreen, 'normal} -> Number 
(define (main x)
  (big-bang 900
    (to-draw (lambda (x) (circle (+ 20 x) 'solid 'red)))
    (on-tick (lambda (x) (- x 20)))
    (stop-when (lambda (x) (< x 0)))
    (close-on-stop #true)
    (display-mode x)))

(main 'fullscreen)
