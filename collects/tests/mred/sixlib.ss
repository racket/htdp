
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
  
(local [(define width 500)
	(define height 500)
	(define pixmap-filename (build-path (collection-path "icons")
					    "plt.gif"))
	(define view-port (open-viewport "pixmap tests" width height))
	(define (line)
	  ((draw-line view-port) (make-posn 50 50) (make-posn 450 450)))
	(define (next desc)
	  ((draw-string view-port) (make-posn 0 (- height 50)) desc)
	  ((draw-string view-port) (make-posn 0 (- height 30)) "click to continue")
	  (get-mouse-click view-port)
	  (write 1)
	  ((clear-viewport view-port))
	  (write 2))]
  (local [(define-values (draw clear flip) (pixmap-functions pixmap-filename))
	  (define (primitive f name)
	    (line)
	    (f view-port (make-posn 0 0))
	    (next (format "pixmap-functions: draw line then ~a pixmap" name)))]
    
    (primitive draw "draw")
    (primitive clear "clear")
    (primitive flip "flip"))
  (local [(define (simple f name)
	    (line)
	    (((f pixmap-filename) view-port) (make-posn 0 0))
	    (next (format "draw line then ~a-pixmap-posn" name)))]
    
    (simple draw-pixmap-posn  "draw")
    (simple clear-pixmap-posn "clear")
    (simple flip-pixmap-posn "flip"))
  (close-viewport view-port))

(close-graphics)

; gtest-snip
