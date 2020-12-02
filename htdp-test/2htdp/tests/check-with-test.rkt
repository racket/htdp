#lang racket 

;; makes sure check-with produces a good error message
;; (I am clueless as to why this broke.)

(require 2htdp/image)
(require 2htdp/universe)

(define (trivial x)
  (overlay x (empty-scene 300 200)))

(define msg
  #px"the initial expression evaluated to .*, which fails to pass check-with's number\\? test")

(with-handlers ([exn:fail? (lambda (xn) (unless (regexp-match msg (exn-message xn)) (raise xn)))])
  (big-bang (circle 10 "solid" "blue")
    (check-with number?)
    (on-draw trivial)))
