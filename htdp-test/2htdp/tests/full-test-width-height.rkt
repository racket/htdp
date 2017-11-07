#lang htdp/asl

(require 2htdp/universe)
(require 2htdp/image)

(define-struct world [size width height])
;; World = (make-world Number [Maybe Number] [Maybe Number])

;; World Number -> World 
(define (update-size w s)
  (make-world s (world-width w) (world-height w)))

;; {'fullscreen, 'normal} -> Number 
(define (main x)
  (big-bang (make-world 900 #false #false)
    (to-draw (lambda (x)
	       (local ((define width (world-width x)))
		 (if (boolean? width)
		     (circle (+  20 (world-size x)) 'solid 'red)
		     (overlay
		       (rectangle (* .20 width) (* .20 (world-height x)) 'solid 'blue)
		       (circle (+  20 (world-size x)) 'solid 'red))))))
    (on-tick   (lambda (x) (update-size x (- (world-size x) 20))))
    (stop-when (lambda (w) (< (world-size w) 0)))
    (display-mode x
      ;; optional function to pick up the extent of the world 
      (lambda (w width height) (make-world (world-size w) width height)))
    (close-on-stop #true)))

(main 'fullscreen)
(main 'normal)
