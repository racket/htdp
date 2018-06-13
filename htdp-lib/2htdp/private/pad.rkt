#lang racket/gui

;; provide basic elements for game pad clause in big-bang: the icon, pad-event?

(require mrlib/include-bitmap)

(provide
  ;; bitmap
  game-pad
  ;; KeyEvent -> Boolean
  ;; is the given key-event also a pad-event? 
  pad-event? 
  ;; PadEvent PadEvent -> Boolean 
  ;; are the two pad-events equal? 
  pad=?
  )

;; ---------------------------------------------------------------------------------------------------

(define game-pad (include-bitmap "gamepad.png" 'png/alpha))
(unless (send game-pad ok?)
  (error 'big-bang "the game pad icon isn't available; please report error"))

(define pad-buttons
  '("up"    "w" 
    "down"  "s" 
    "left"  "a"
    "right" "d"
    " "
    "shift" "rshift"))

(define (pad-event? ke)
  (pair? (member ke pad-buttons)))

(define (pad=? ke kq)
  (and (pad-event? ke) (pad-event? kq) (string=? ke kq)))
