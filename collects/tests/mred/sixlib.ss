
(define gtest-snip (void))

(open-graphics)
(let ([v (open-viewport "Tester" 100 100)])
  ((draw-string v) (make-posn 0 20) "Reversed X; click to continue")
  ((draw-line v) (make-posn 0 0) (make-posn 100 100))
  ((draw-line v) (make-posn 100 0) (make-posn 0 100))
  ((flip-viewport v))
  (get-mouse-click v)

  ((clear-viewport v))
  ((draw-string v) (make-posn 0 20) "Cleared; click")
  (get-mouse-click v)

  (let ([rect-draw
	 (lambda (f)
	   (f (make-posn 20 20) 60 60))]
	[poly-draw
	 (lambda (f)
	   (f (list (make-posn 0 0) (make-posn 40 0) (make-posn 20 40)) (make-posn 20 20)))]
	[shape
	 (lambda (do-draw draw clear flip name)
	   ((clear-viewport v))
	   ((draw-string v) (make-posn 0 20) (format "Centered ~s" name))
	   (do-draw (draw v))
	   (get-mouse-click v)

	   ((draw-string v) (make-posn 0 40) (format "Erased ~s" name))
	   (do-draw (clear v))
	   (get-mouse-click v)

	   ((clear-viewport v))
	   ((draw-string v) (make-posn 0 20) (format "Centered ~s" name))
	   (do-draw (draw v))
	   (get-mouse-click v)

	   ((draw-string v) (make-posn 0 40) (format "Flipped ~s" name))
	   (do-draw (flip v))
	   (get-mouse-click v)

	   ((draw-string v) (make-posn 0 40) (format "Flipped ~s back" name))
	   (do-draw (flip v))
	   (get-mouse-click v))])
    (shape rect-draw draw-rectangle clear-rectangle flip-rectangle "box")
    (shape rect-draw draw-solid-rectangle clear-solid-rectangle flip-solid-rectangle "solid box")
    (shape rect-draw draw-ellipse clear-ellipse flip-ellipse "circle")
    (shape rect-draw draw-solid-ellipse clear-solid-ellipse flip-solid-ellipse "solid circle")
    (shape poly-draw draw-polygon clear-polygon flip-polygon "polygon")
    (shape poly-draw draw-solid-polygon clear-solid-polygon flip-solid-polygon "solid polygon"))

  ((clear-viewport v))
  ((draw-string v) (make-posn 0 20) "Done; click")
  (get-mouse-click v)
  
  (set! gtest-snip (viewport->snip v))

  (close-viewport v))
  
(close-graphics)

; gtest-snip
