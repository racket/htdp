;; This tests a good portion but needs some more 
;; This needs some tests for error behavior of functions ... 

;; draw-next-part : symbol -> true
;; consumes one of the seven body-part symbols and draws that part.

(define (draw-next-part body-part)
  (cond 
    [(eq? body-part 'body)
     (draw-solid-line (make-posn 100 60)
                      (make-posn 100 130)
                      BLACK)]
    [(eq? body-part 'right-leg)
     (draw-solid-line (make-posn 100 130)
                      (make-posn 30 170)
                      BLACK)]
    [(eq? body-part 'left-leg)
     (draw-solid-line (make-posn 100 130)
                      (make-posn 170 170)
                      BLACK)]
    [(eq? body-part 'right-arm)
     (draw-solid-line (make-posn 100 75)
                      (make-posn 40 65)
                      BLACK)]
    [(eq? body-part 'left-arm)
     (draw-solid-line (make-posn 100 75)
                      (make-posn 160 65)
                      BLACK)]
    [(eq? body-part 'head)
     (draw-solid-disk (make-posn 100 50) 10 BLACK)]
    [(eq? body-part 'noose)
     (and
      (draw-solid-disk (make-posn 120 50) 30 RED)
      (draw-solid-line (make-posn 100 30)
                       (make-posn 100 10)
                       BLACK)
      (draw-solid-line (make-posn 100 10)
                       (make-posn 0 10)
                       BLACK)
      (draw-solid-line (make-posn 115 35)
                       (make-posn 123 43)
                       BLACK)
      (draw-solid-line (make-posn 123 35)
                       (make-posn 115 43)
                       BLACK)
      (draw-solid-line (make-posn 131 40)
                       (make-posn 139 48)
                       BLACK)
      (draw-solid-line (make-posn 139 40)
                       (make-posn 131 48)
                       BLACK))]))

#| Tests ----------------------------------------------------------

WHITE YELLOW RED BLUE GREEN BLACK

(start 200 400)
(sleep-for-a-while 1)
(draw-next-part 'noose)
(sleep-for-a-while 1)
(draw-next-part 'head)
(sleep-for-a-while 1)
(draw-next-part 'left-arm)
(sleep-for-a-while 1)
(draw-next-part 'right-arm)
(sleep-for-a-while 1)
(draw-next-part 'body)
(sleep-for-a-while 1)
(draw-next-part 'left-leg)
(sleep-for-a-while 1)
(draw-next-part 'right-leg)
|#

(load "tester.ss")
(start 200 400)
(test-error (draw-solid-line 'a 'b 'c))
