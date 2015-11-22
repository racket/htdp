#lang htdp/isl+

(require 2htdp/universe)
(require 2htdp/image)

(define (test close? final)
  (big-bang 3
    (to-draw (lambda (x) (circle (+ (* x 10) 100) 'solid 'red)))
    (on-tick sub1)
    (stop-when zero? final)
    (close-on-stop close?)))

(test #false (lambda (x) (text "this one remained open" 22 'black)))
(test 3 (lambda (x) (text "this one will close on its own" 22 'green)))
  