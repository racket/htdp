#lang htdp/bsl
(require 2htdp/image)
(require 2htdp/universe)

;; nat-main : Natural -> Natural
(define (nat-main t)
  (big-bang t
    [on-tick add1]
    [on-key nat-handle-key]
    [to-draw draw-circle]))

(check-big-bang (nat-main 0)
  [(make-tick) 1]
  [(make-tick) 2]
  [(make-tick) 3]
  [(make-key "left") 2]
  [(make-tick) 3]
  [(make-key "right") 4]
  [(make-tick) 5]
  [(make-key "left") 4]
  [(make-key "left") 3]
  [(make-key "left") 2]
  [(make-key "left") 1]
  [(make-key "left") 0]
  [(make-key "left") 0]
  [(make-tick) 1])

;; draw-circle : PosNum -> Image
(define (draw-circle r)
  (circle r "solid" "red"))

;; handle-key : Natural KeyEvent -> Natural
(define (nat-handle-key t k)
  (cond [(key=? k "left") (max 0 (sub1 t))]
        [(key=? k "right") (add1 t)]
        [else t]))

;; posn-main : Posn -> Posn
;; Lanches a big-bang window
(define (posn-main p)
  (big-bang p
    [on-tick move-posn]
    [on-key posn-handle-key]
    [on-mouse posn-handle-mouse]
    [to-draw display-ball]))

(check-big-bang (posn-main (make-posn 0 0))
  [(make-tick)        (make-posn 1 1)]
  [(make-key "right") (make-posn 6 1)]
  [(make-tick)        (make-posn 7 2)]
  [(make-key "left")  (make-posn 2 2)]
  [(make-key "down")  (make-posn 2 7)]
  [(make-tick)        (make-posn 3 8)]
  [(make-mouse 100 50 "button-down") (make-posn 100 50)]
  [(make-tick)        (make-posn 101 51)])

(check-big-bang* (posn-main (make-posn 0 0))
  (list
   (make-event-expect (make-tick)        (make-posn 1 1))
   (make-event-expect (make-key "right") (make-posn 6 1))
   (make-event-expect (make-tick)        (make-posn 7 2))
   (make-event-expect (make-key "left")  (make-posn 2 2))
   (make-event-expect (make-key "down")  (make-posn 2 7))
   (make-event-expect (make-tick)        (make-posn 3 8))
   (make-event-expect (make-mouse 100 50 "button-down") (make-posn 100 50))
   (make-event-expect (make-tick)        (make-posn 101 51))))


;; move-posn : Posn -> Posn
(define (move-posn p)
  (make-posn (+ (posn-x p) 1)
             (+ (posn-y p) 1)))

;; posn-handle-key : Posn Key -> Posn
(define (posn-handle-key p k)
  (cond [(key=? k "left")  (make-posn (- (posn-x p) 5) (posn-y p))]
        [(key=? k "right") (make-posn (+ (posn-x p) 5) (posn-y p))]
        [(key=? k "up")    (make-posn (posn-x p) (- (posn-y p) 5))]
        [(key=? k "down")  (make-posn (posn-x p) (+ (posn-y p) 5))]
        [else p]))

;; posn-handle-mouse : Posn Integer Integer Mouse-Event -> Posn
(define (posn-handle-mouse p x y m)
  (cond [(mouse=? m "button-down") (make-posn x y)]
        [else p]))

;; display-ball : Posn -> Image
(define BG (empty-scene 500 500))
(define (display-ball p)
  (place-image (circle 20 "solid" "blue")
               (posn-x p) (posn-y p)
               BG))

