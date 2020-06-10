#lang racket/gui

(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)

(define ground 350)
(define-struct ufo (height velocity))

(define UFO
  (underlay/align "center"
                  "center"
                  (circle 10 "solid" "green")
                  (rectangle 40 4 "solid" "green")))

(define explosion (star-polygon 20 10 3 "solid" "red"))

(define (create-UFO-scene my-ufo)
  (underlay/xy (rectangle 100 100 "solid" "white") 50 (ufo-height my-ufo) UFO))

(define (last-picture my-ufo)
  (underlay/xy (rectangle 100 100 "solid" "white") 50 (ufo-height my-ufo) explosion))

(define (stop-expr my-ufo last-picture)
  (if (>= (ufo-height my-ufo) ground) #t #f))

(check-exn
 #px"stop-when: expected function of one argument as first argument; given function of 2 arguments"
 (lambda ()
   (big-bang (make-ufo 0 1) ;start state
     (to-draw create-UFO-scene 400 400)
     (stop-when stop-expr last-picture))))

(displayln "hilite last claise, and repor")

#;
(big-bang (make-ufo 0 1) ;start state
     (to-draw create-UFO-scene 400 400)
     (stop-when stop-expr last-picture))
