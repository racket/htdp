; graphics.ss
; Simple graphics routines for MrEd
; Originally written by Johnathan Franklin

(plt:require-library "graphics.ss")

(define graphics@
  (unit/sig
   graphics^
   (import)

   (define-struct viewport (label canvas))
   (define-struct posn (x y))
   (define-struct sixmouse (x y left? middle? right?))
   (define-struct sixkey (value))
   (define graphics-flag #f)
   (define GLOBAL-VIEWPORT-LIST '())
   (define GLOBAL-COLOR-VECTOR (make-vector 300))
   (define GLOBAL-PEN-VECTOR (make-vector 300))
   (define GLOBAL-BRUSH-VECTOR (make-vector 300))

   (define FRAME-W-DELTA 0)
   (define FRAME-H-DELTA 0)
   (define CANVAS-W-DELTA 0)
   (define CANVAS-H-DELTA 0)

    (when (eq? wx:window-system 'xt)
      (set! FRAME-W-DELTA 16)
      (set! FRAME-H-DELTA 16)
      (set! CANVAS-W-DELTA 12)
      (set! CANVAS-H-DELTA 12))

   (define wx:sixlib-canvas%
     (class-asi wx:canvas% 
		(inherit get-parent set-size)
		(private
		 (current-mouse-posn (make-posn 0 0))
		 (queue%
		  (make-class ()
			      (private
			       [queue '()]
			       [last #f])
			      (public
			       [flush
				(lambda ()
				  (set! queue '())
				  (set! last #f))]
			       [add
				(lambda (v)
				  (if last
				      (begin
					(set-cdr! last (cons v '()))
					(set! last (cdr last)))
				      (begin
					(set! queue (cons v '()))
					(set! last queue))))]
			       [remove
				(lambda ()
				  (if (null? queue)
				      #f
				      (begin0
				       (car queue)
				       (set! queue (cdr queue))
				       (if (null? queue)
					   (set! last #f)))))])))
		 (click-queue (make-object queue%))
		 (release-queue (make-object queue%))
		 (press-queue (make-object queue%))
		 (reset-size
		  (lambda ()
		    (let ([width (* scale width)]
			  [height (* scale height)])
		      (let ([f (get-parent)])
			(send f set-size -1 -1 
			      (+ FRAME-W-DELTA width)
			      (+ FRAME-H-DELTA height)))
		      (set-size -1 -1 
				(+ CANVAS-W-DELTA width) 
				(+ CANVAS-H-DELTA height))
		      (set! bitmap (make-object wx:bitmap% width height))
		      (send buffer-DC select-object bitmap)
		      (send buffer-DC set-brush (send DC get-brush))
		      (send buffer-DC set-pen (send DC get-pen))
		      (let ([f (send DC get-font)])
			(unless (null? f)
				(send buffer-DC set-font f)))
		      (send buffer-DC clear)
		      (send DC clear)))))
		
		
		(public
		 viewport
		 (scale 1.0)
		 (height 0)
		 (width 0)
		 (label 0)
		 current-pen
		 current-brush
		 bitmap
		 DC
		 buffer-DC
		 (get-current-pen (lambda () current-pen))
		 (get-current-brush (lambda () current-brush))
		 (remember-pen (lambda (pen) (set! current-pen pen)))
		 (remember-brush (lambda (brush) (set! current-brush brush)))
		 (leaving-event
		  (make-object wx:mouse-event% wx:const-event-type-leave-window))
		 
		 (on-paint
		  (lambda ()
		    (send DC blit 0 0 width height buffer-DC 0 0 wx:const-copy)))

		 (on-event 
		  (lambda (mouse-event)
		    (let* ([x (send mouse-event get-x)]
			   [y (send mouse-event get-y)]
			   [left? (send mouse-event button-down? 1)]
			   [middle? (send mouse-event button-down? 2)]
			   [right? (send mouse-event button-down? 3)]
			   [sixm (make-sixmouse x y left? middle? right?)])
		      (set! current-mouse-posn (make-posn x y))
		      (cond
		       [(send mouse-event button-down?) 
			(send click-queue add sixm)]
		       [(send mouse-event button-up?)
			(send release-queue add sixm)]
		       [else (void)]))))

		 (on-char
		  (lambda (key-event)
		    (send press-queue add (make-sixkey (send key-event key-code)))))

		 (get-click
		  (lambda ()
		    (send click-queue remove)))
		 
		 (get-release
		  (lambda ()
		    (send release-queue remove)))

		 (get-press
		  (lambda ()
		    (send press-queue remove)))

		 (get-posn (lambda () current-mouse-posn))
		 (set-DC (lambda (new-DC) (set! DC new-DC)))
		 (set-buffer-DC (lambda (new-buffer-DC) (set! buffer-DC
							      new-buffer-DC)))

		 (set-geometry
		  (lambda (new-width new-height new-scale)
		    (set! height new-height)
		    (set! width new-width)
		    (set! scale new-scale)
		    (reset-size)))
		 (set-height (lambda (new-height) 
			       (set! height new-height)
			       (reset-size)))
		 (set-width (lambda (new-width) 
			      (set! width new-width)
			      (reset-size)))
		 (set-scale (lambda (new-scale)
			      (set! scale new-scale)
			      (send DC set-user-scale scale scale)
			      (send buffer-DC set-user-scale scale scale)
			      (reset-size)))
		 
		 
		 (viewport-flush-input
		  (lambda ()
		    (send click-queue flush)
		    (send release-queue flush)
		    (send press-queue flush))))))

   (define repaint
     (lambda (viewport)
       (send (viewport-canvas viewport) on-paint)))

   (define viewport-DC 
     (lambda (viewport)
       (ivar (viewport-canvas viewport) DC)))

   (define viewport-buffer-DC
     (lambda (viewport)
       (ivar (viewport-canvas viewport) buffer-DC)))

   (define viewport-frame
     (lambda (viewport)
       (send (viewport-canvas viewport) get-parent)))

   (define viewport-height
     (lambda (viewport)
       (ivar (viewport-canvas viewport) height)))

   (define viewport-width
     (lambda (viewport)
       (ivar (viewport-canvas viewport) width)))

   (define arrow-cursor
     (make-object wx:cursor% wx:const-cursor-arrow))


   (define (get-mouse-click viewport) 
     (let*
	 ([status (ready-mouse-click viewport)]
	  [icon-change
	   (send (viewport-canvas viewport) set-cursor arrow-cursor)])
       (cond
	[status status]
	[else (wx:yield) (get-mouse-click viewport)])))
   

   (define (get-key-press viewport) 
     (let*
	 ([status (ready-key-press viewport)])
       (cond
	[status status]
	[else (wx:yield) (get-key-press viewport)])))

   (define ready-mouse-click
     (lambda (viewport) (send (viewport-canvas viewport) get-click)))

   (define ready-mouse-release
     (lambda (viewport) (send (viewport-canvas viewport) get-release)))

   (define ready-key-press
     (lambda (viewport) (send (viewport-canvas viewport) get-press)))

   (define mouse-click-posn
     (lambda (mouse-event)
       (make-posn (sixmouse-x mouse-event) (sixmouse-y mouse-event))))

   (define query-mouse-posn
     (lambda (viewport) (send (viewport-canvas viewport) get-posn)))

   (define viewport-flush-input
     (lambda (viewport) (send (viewport-canvas viewport) viewport-flush-input)))

   (define left-mouse-click?
     (lambda (mouse-event) (sixmouse-left? mouse-event)))

   (define middle-mouse-click?
     (lambda (mouse-event) (sixmouse-middle? mouse-event)))

   (define right-mouse-click?
     (lambda (mouse-event) (sixmouse-right? mouse-event)))

   (define key-value sixkey-value)

   (define clear-viewport
     (lambda (viewport)
       (let* ([clear (ivar (viewport-DC viewport) clear)]
	      [clear2 (ivar (viewport-buffer-DC viewport) clear)])
	 (lambda ()
	   (clear)
	   (clear2)))))

   (define draw-viewport
     (lambda (viewport)
       (let* ([draw (ivar (viewport-DC viewport) draw-rectangle)]
	      [draw2 (ivar (viewport-buffer-DC viewport) draw-rectangle)]
	      [w (viewport-width viewport)]
	      [h (viewport-height viewport)])
	 (lambda ()
	   (draw 0 0 w h)
	   (draw2 0 0 w h)))))

   (define flip-viewport
     (lambda (viewport)
       (let* ([dc (viewport-DC viewport)]
	      [dc2 (viewport-buffer-DC viewport)]
	      [set-pen (ivar dc set-pen)]
	      [set-pen2 (ivar dc2 set-pen)]
	      [draw (ivar dc draw-rectangle)]
	      [draw2 (ivar dc2 draw-rectangle)]
	      [set-lf (ivar dc set-logical-function)]
	      [set-lf2 (ivar dc2 set-logical-function)]
	      [w (viewport-width viewport)]
	      [h (viewport-height viewport)])
	 (lambda ()
	   (let ([pen (send dc get-pen)])
	     (set-lf wx:const-xor)
	     (set-lf2 wx:const-xor)
	     (set-pen invisi-pen)
	     (set-pen2 invisi-pen)
	     (draw 0 0 w h)
	     (draw2 0 0 w h)
	     (set-pen pen)
	     (set-pen2 pen)
	     (set-lf wx:const-copy)
	     (set-lf2 wx:const-copy))))))


   (define close-viewport
     (lambda (viewport)
       (set! GLOBAL-VIEWPORT-LIST 
	     (let loop ([l GLOBAL-VIEWPORT-LIST])
	       (cond
		[(null? l) '()]
		[(eq? (car l) viewport) (cdr l)]
		[else (cons (car l) (loop (cdr l)))])))
       (send (viewport-frame viewport) show #f)
       (send (viewport-canvas viewport) show #f)))

   (define open-graphics 
     (lambda () 
       (set! graphics-flag #t)))

   (define close-graphics 
     (lambda ()
       (map close-viewport GLOBAL-VIEWPORT-LIST)
       (set! graphics-flag #f)
       (set! GLOBAL-VIEWPORT-LIST '())))

   (define graphics-open? (lambda () graphics-flag))

   (define make-rgb
     (lambda (red green blue)
       (when (or (< red 0.) (< blue 0.) (< green 0.)
		 (> red 1.) (> blue 1.) (> green 1.))
	     (error 'make-rgb
		    "all color indices should be in [0.0, 1.0]; provided ~s"
		    (list red green blue)))
       (let* ([convert (lambda (num) (inexact->exact (round (* 255 num))))]
	      [nred (convert red)]
	      [ngreen (convert green)]
	      [nblue (convert blue)])
	 (make-object wx:colour% nred ngreen nblue))))

   (define make-color make-rgb)

   (define get-rgb-map
     (lambda (rgb)
       (let*
	   ([r-box (box 10)]
	    [g-box (box 10)]
	    [b-box (box 10)])
	 (send rgb get r-box g-box b-box)
	 (list (/ (unbox r-box) 255.0)
	       (/ (unbox g-box) 255.0)
	       (/ (unbox b-box) 255.0)))))

   (define rgb-blue (lambda (rgb) (cadr (get-rgb-map rgb))))
   (define rgb-red (lambda (rgb) (car (get-rgb-map rgb))))
   (define rgb-green (lambda (rgb) (caddr (get-rgb-map rgb))))

   (define change-color
     (lambda (index color)
       (vector-set! GLOBAL-COLOR-VECTOR index color)
       (vector-set! GLOBAL-PEN-VECTOR index (get-pen color))
       (vector-set! GLOBAL-BRUSH-VECTOR index (get-brush color))))

   (define get-color
     (lambda (index)
       (cond
	[(is-a? index wx:colour%) index]
	[(string? index) (make-object wx:colour% index)]
	[else (vector-ref GLOBAL-COLOR-VECTOR index)])))

   (define get-pen
     (lambda (index)
       (cond
	[(is-a? index wx:pen%) index]
	[(or (string? index) (is-a? index wx:colour%))
	 (make-object wx:pen% index 1 wx:const-solid)]
	[else (vector-ref GLOBAL-PEN-VECTOR index)])))

   (define get-brush
     (lambda (index)
       (cond
	[(is-a? index wx:brush%) index]
	[(or (string? index) (is-a? index wx:colour%))
	 (make-object wx:brush% index wx:const-solid)]
	[else (vector-ref GLOBAL-BRUSH-VECTOR index)])))

   (define rgb? (lambda (object) (is-a? object wx:colour%)))
   (define color? rgb?)
   (define pen? (lambda (object) (is-a? object wx:pen%)))
   (define brush? (lambda (object) (is-a? object wx:brush%)))

   (define display-color-vector
     (lambda ()
       (do
	   ([index 0 (+ index 1)])
	   ((eq? index 100))
	 (display (get-rgb-map (get-color index))))))

   (define make-font
     (lambda (name)
       (cond
	[(eq? name 'large-deco)
	 (make-object 
	  wx:font% 40 wx:const-decorative wx:const-normal wx:const-normal)]
	[(eq? name 'small-roman)
	 (make-object 
	  wx:font% 13 wx:const-roman wx:const-normal wx:const-normal)]
	[(eq? name 'medium-roman)
	 (make-object 
	  wx:font% 25 wx:const-roman wx:const-normal wx:const-normal)]
	[(eq? name 'large-roman)
	 (make-object 
	  wx:font% 40 wx:const-roman wx:const-normal wx:const-normal)]
	[else "no such font"]
	)))

   (define custom-roman
     (lambda (size)
       (make-object
	wx:font% size wx:const-roman wx:const-normal wx:const-normal)))

   (define custom-deco
     (lambda (size)
       (make-object
	wx:font% size wx:const-decorative wx:const-normal
	wx:const-normal)))

   (define set-viewport-pen
     (lambda (viewport pen)
       (send (viewport-canvas viewport) remember-pen pen)
       (let ([pen (get-pen pen)])
	 (send (viewport-DC viewport) set-pen pen)
	 (send (viewport-buffer-DC viewport) set-pen pen))))
   
   (define set-viewport-brush
     (lambda (viewport brush)
       (send (viewport-canvas viewport) remember-brush brush)
       (let ([brush (get-brush brush)])
	 (send (viewport-DC viewport) set-brush brush)
	 (send (viewport-buffer-DC viewport) set-brush brush))))

   (define set-text-foreground
     (lambda (viewport color)
       (let ([color (get-color color)])
	 (send (viewport-DC viewport) set-text-foreground color)
	 (send (viewport-buffer-DC viewport) set-text-foreground color))))

   (define set-text-background
     (lambda (viewport color)
       (let ([color (get-color color)])
	 (send (viewport-DC viewport) set-text-background color)
	 (send (viewport-buffer-DC viewport) set-text-background color))))

   (define set-viewport-font
     (lambda (viewport font)
       (send (viewport-DC viewport) set-font font)
       (send (viewport-buffer-DC viewport) set-font font)))
   
   (define set-viewport-background
     (lambda (viewport brush)
       (let ([color (get-brush brush)])
	 (send (viewport-DC viewport) set-background brush)
	 (send (viewport-buffer-DC viewport) set-background brush))))

   (define set-viewport-logical-function
     (lambda (viewport logical-function)
       (send (viewport-DC viewport) set-logical-function logical-function)
       (send (viewport-buffer-DC viewport) set-logical-function
	     logical-function)))

   (define set-viewport-scale
     (lambda (viewport scale)
       (send (viewport-canvas viewport) set-scale scale)))

   (define white (make-rgb 1 1 1))
   (define black (make-rgb 0 0 0))
   (define red (make-rgb 1 0 0))
   (define green (make-rgb 0 1 0))
   (define blue (make-rgb 0 0 1))
   (define white-pen (get-pen white))
   (define black-pen (get-pen black))
   (define red-pen (get-pen red))
   (define blue-pen (get-pen blue))
   (define green-pen (get-pen green))
   (define white-brush (get-brush white))
   (define black-brush (get-brush black))
   (define red-brush (get-brush red))
   (define green-brush (get-brush green))
   (define blue-brush (get-brush blue))

   (define invisi-pen (make-object wx:pen% "WHITE" 1 wx:const-transparent))
   (define invisi-brush (make-object wx:brush% "WHITE" wx:const-transparent))

   (define draw-it (lambda (draw flip clear) (draw)))
   (define flip-it (lambda (draw flip clear) (flip)))
   (define clear-it (lambda (draw flip clear) (clear)))

   (define make-draw-proc
     (lambda (draw-name get-pen-name set-pen-name 
			get-current-pen-name set-viewport-pen white-pen)
       (lambda (viewport) 
	 (let ([current-pen (uq-ivar (viewport-canvas viewport) get-current-pen-name)]
	       [draw-1 (uq-ivar (viewport-DC viewport) draw-name)]
	       [draw-2 (uq-ivar (viewport-buffer-DC viewport) draw-name)]
	       [get-pen (uq-ivar (viewport-DC viewport) get-pen-name)]
	       [set-pen-1 (uq-ivar (viewport-DC viewport) set-pen-name)]
	       [set-pen-2 (uq-ivar (viewport-buffer-DC viewport) set-pen-name)]
	       [set-logical-function-1 (ivar (viewport-DC viewport) set-logical-function)]
	       [set-logical-function-2 (ivar (viewport-buffer-DC viewport) set-logical-function)])
	   (lambda (color go)
	     (when (and color (not (eq? color (current-pen))))
		   (set-viewport-pen viewport color))
	     (go draw-1 draw-2
		 (lambda (draw)
		   (set-logical-function-1 wx:const-xor)
		   (set-logical-function-2 wx:const-xor)
		   (draw)
		   (set-logical-function-1 wx:const-copy)
		   (set-logical-function-2 wx:const-copy))
		 (lambda (draw)
		   (let ([pen (get-pen)])
		     (set-pen-1 white-pen)
		     (set-pen-2 white-pen)
		     (draw)
		     (set-pen-1 pen)
		     (set-pen-2 pen)))))))))
   
   (define make-do-line
     (lambda (go)
       (let ([f (make-draw-proc 'draw-line 'get-pen 'set-pen 
				'get-current-pen set-viewport-pen white-pen)])
	 (lambda (viewport)
	   (let ([f (f viewport)])
	     (letrec ([the-function
		       (case-lambda
			[(posn1 posn2) (the-function posn1 posn2 #f)]
			[(posn1 posn2 color)
			 (f color
			    (lambda (draw-1 draw-2 flip clear)
			      (let* ([x1 (posn-x posn1)]
				     [y1 (posn-y posn1)]
				     [x2 (posn-x posn2)]
				     [y2 (posn-y posn2)]
				     [draw (lambda ()
					     (draw-1 x1 y1 x2 y2)
					     (draw-2 x1 y1 x2 y2))])
				(go draw
				    (lambda () (flip draw))
				    (lambda () (clear draw))))))])])
	       the-function))))))

   (define draw-line (make-do-line draw-it))
   (define clear-line (make-do-line clear-it))
   (define flip-line (make-do-line flip-it))

   (define make-do-box
     (lambda (go name get-pen-name set-pen-name
		 get-current-pen-name set-viewport-pen white-pen
		 get-brush-name set-brush-name invisi-brush)
       (let ([f (make-draw-proc name get-pen-name set-pen-name 
				get-current-pen-name set-viewport-pen white-pen)])
	 (lambda (viewport)
	   (let ([f (f viewport)]
		 [get-brush (uq-ivar (viewport-DC viewport) get-brush-name)]
		 [set-brush-1 (uq-ivar (viewport-DC viewport) set-brush-name)]
		 [set-brush-2 (uq-ivar (viewport-buffer-DC viewport) set-brush-name)])
	     (letrec ([the-function
		       (case-lambda
			[(posn width height) (the-function posn width height #f)]
			[(posn width height color)
			 (f color
			    (lambda (draw-1 draw-2 flip clear)
			      (let* ([x (posn-x posn)]
				     [y (posn-y posn)]
				     [orig (get-brush)]
				     [draw (lambda ()
					     (set-brush-1 invisi-brush)
					     (set-brush-2 invisi-brush)
					     (draw-1 x y width height)
					     (draw-2 x y width height)
					     (set-brush-1 orig)
					     (set-brush-2 orig))])
				(go draw
				    (lambda () (flip draw))
				    (lambda () (clear draw))))))])])
	       the-function))))))

   (define make-do-rectangle
     (lambda (go)
       (make-do-box go 'draw-rectangle 'get-pen 'set-pen 
		    'get-current-pen set-viewport-pen white-pen
		    'get-brush 'set-brush invisi-brush)))

   (define make-do-solid-rectangle
     (lambda (go)
       (make-do-box go 'draw-rectangle 'get-brush 'set-brush  
		    'get-current-brush set-viewport-brush white-brush
		    'get-pen 'set-pen invisi-pen)))

   (define make-do-ellipse
     (lambda (go)
       (make-do-box go 'draw-ellipse 'get-pen 'set-pen  
		    'get-current-pen set-viewport-pen white-pen
		    'get-brush 'set-brush invisi-brush)))

   (define make-do-solid-ellipse
     (lambda (go)
       (make-do-box go 'draw-ellipse 'get-brush 'set-brush   
		    'get-current-brush set-viewport-brush white-brush
		    'get-pen 'set-pen invisi-pen)))

   (define draw-rectangle (make-do-rectangle draw-it))
   (define clear-rectangle (make-do-rectangle clear-it))
   (define flip-rectangle (make-do-rectangle flip-it))

   (define draw-solid-rectangle (make-do-solid-rectangle draw-it))
   (define clear-solid-rectangle (make-do-solid-rectangle clear-it))
   (define flip-solid-rectangle (make-do-solid-rectangle flip-it))

   (define draw-ellipse (make-do-ellipse draw-it))
   (define clear-ellipse (make-do-ellipse clear-it))
   (define flip-ellipse (make-do-ellipse flip-it))

   (define draw-solid-ellipse (make-do-solid-ellipse draw-it))
   (define clear-solid-ellipse (make-do-solid-ellipse clear-it))
   (define flip-solid-ellipse (make-do-solid-ellipse flip-it))

   (define make-do-pointlist
     (lambda (go name get-pen-name set-pen-name
		 get-current-pen-name set-viewport-pen white-pen
		 get-brush-name set-brush-name invisi-brush)
       (let ([f (make-draw-proc name get-pen-name set-pen-name 
				get-current-pen-name set-viewport-pen white-pen)])
	 (lambda (viewport)
	   (let ([f (f viewport)]
		 [get-brush (uq-ivar (viewport-DC viewport) get-brush-name)]
		 [set-brush-1 (uq-ivar (viewport-DC viewport) set-brush-name)]
		 [set-brush-2 (uq-ivar (viewport-buffer-DC viewport) set-brush-name)])
	     (letrec ([the-function
		       (case-lambda
			[(posns offset) (the-function posns offset #f)]
			[(posns offset color)
			 (f color
			    (lambda (draw-1 draw-2 flip clear)
			      (let* ([points (map (lambda (p)
						    (make-object wx:point% (posn-x p) (posn-y p)))
						  posns)]
				     [x (posn-x offset)]
				     [y (posn-y offset)]
				     [orig (get-brush)]
				     [draw (lambda ()
					     (set-brush-1 invisi-brush)
					     (set-brush-2 invisi-brush)
					     (draw-1 points x y)
					     (draw-2 points x y)
					     (set-brush-1 orig)
					     (set-brush-2 orig))])
				(go draw
				    (lambda () (flip draw))
				    (lambda () (clear draw))))))])])
	       the-function))))))

   (define make-do-polygon
     (lambda (go)
       (make-do-pointlist go 'draw-polygon 'get-pen 'set-pen 
			  'get-current-pen set-viewport-pen white-pen
			  'get-brush 'set-brush invisi-brush)))

   (define make-do-solid-polygon
     (lambda (go)
       (make-do-pointlist go 'draw-polygon 'get-brush 'set-brush  
			  'get-current-brush set-viewport-brush white-brush
			  'get-pen 'set-pen invisi-pen)))

   (define draw-polygon (make-do-polygon draw-it))
   (define clear-polygon (make-do-polygon clear-it))
   (define flip-polygon (make-do-polygon flip-it))

   (define draw-solid-polygon (make-do-solid-polygon draw-it))
   (define clear-solid-polygon (make-do-solid-polygon clear-it))
   (define flip-solid-polygon (make-do-solid-polygon flip-it))

   (define make-do-pixel
     (lambda (go)
       (let ([f (make-draw-proc 'draw-point'get-pen 'set-pen  
				'get-current-pen set-viewport-pen white-pen)])
	 (lambda (viewport)
	   (let ([f (f viewport)])
	     (letrec ([the-function
		       (case-lambda
			[(posn) (the-function posn #f)]
			[(posn color)
			 (f color
			    (lambda (draw-1 draw-2 flip clear)
			      (let* ([x (posn-x posn)]
				     [y (posn-y posn)]
				     [draw (lambda ()
					     (draw-1 x y)
					     (draw-2 x y))])
				(go draw 
				    (lambda () (flip draw)) 
				    (lambda () (clear draw))))))])])
	       the-function))))))

   (define draw-pixel (make-do-pixel draw-it))
   (define clear-pixel (make-do-pixel clear-it))
   (define flip-pixel (make-do-pixel flip-it))
   
   (define pixmap-functions
     (lambda (pixel-op)
       (lambda (viewport)
	 (letrec ([the-function
		   (case-lambda
		    [(filename posn) (the-function filename posn #f)]
		    [(filename posn color)
		     (let ([bm (make-object wx:bitmap% filename wx:const-bitmap-type-xbm)])
		       (if (not (send bm ok?))
			   (error pixel-op ": cannot load pixmap: ~s" filename))
		       (let* ([offscreen (make-object wx:memory-dc%)]
			      [x (posn-x posn)] 
			      [y (posn-y posn)]
			      [DC (viewport-DC viewport)]
			      [buffer (viewport-buffer-DC viewport)]
			      [picture
			       (lambda (op) 
				 (send offscreen select-object bm)
				 (send DC blit x y
				       (send bm get-width)
				       (send bm get-height)
				       offscreen 0 0 op)
				 (send buffer blit x y
				       (send bm get-width)
				       (send bm get-height)
				       offscreen 0 0 op)
				 (void))])
			 (cond 
			  [(eq? pixel-op 'draw)
			   (if color
			       (set-viewport-pen viewport (get-pen color)))
			   (picture wx:const-copy)] ; should be or
			  [(eq? pixel-op 'flip)
			   (if color
			       (set-viewport-pen viewport (get-pen color)))
			   (picture wx:const-xor)]
			  [(eq? pixel-op 'clear)
			   (picture wx:const-nand)])))])])
	   the-function))))

   (define string-functions
     (lambda (string-op)
       (letrec ([outer-function
		 (case-lambda
		  [(viewport) (outer-function viewport DEFAULT-FONT)]
		  [(viewport font)
		   (letrec ([the-function
			     (case-lambda
			      [(posn text) (the-function posn text #f)]
			      [(posn text color)
			       (let* ([DC (viewport-DC viewport)]
				      [x (posn-x posn)]
				      [h-box (box 0)]
				      [d-box (box 0)]
				      [w-box (box 0)]
				      [junk (send DC get-text-extent "X" 
						  w-box h-box d-box '() font)]
				      [y (- (posn-y posn) (- (unbox h-box) (unbox d-box)))]
				      [buffer (viewport-buffer-DC viewport)]
				      [string-create
				       (lambda ()
					 (send DC draw-text text x y)
					 (send buffer draw-text text x y))])
				 (cond
				  [(eq? string-op 'draw)
				   (if color
				       (set-text-foreground viewport color))
				   (set-viewport-font viewport font)
				   (send DC draw-text text x y)
				   (send buffer draw-text text x y)]
				  [(eq? string-op 'flip)
				   (set-viewport-logical-function viewport wx:const-xor)
				   (if color
				       (set-text-foreground viewport color))
				   (set-viewport-font viewport font)
				   (string-create)
				   (set-viewport-logical-function viewport wx:const-copy)]
				  [(eq? string-op 'clear)
				   (set-text-foreground viewport white)
				   (set-viewport-font viewport font)
				   (string-create)
				   (set-text-foreground viewport black)]))])])
		     the-function)])])
	 outer-function)))

   (define draw-string (string-functions 'draw))
   (define clear-string (string-functions 'clear))
   (define flip-string (string-functions 'flip))

   (define get-string-size
     (letrec ([outer-function
	       (case-lambda
		[(viewport) (outer-function viewport DEFAULT-FONT)]
		[(viewport font)
		 (let ([get-extent (ivar (viewport-DC viewport) get-text-extent)])
		   (lambda (text)
		     (let ([w (box 0)]
			   [h (box 0)])
		       (get-extent text w h null null font)
		       (list (unbox w) (unbox h)))))])])
	 outer-function))

   (define get-pixel 
     (lambda (viewport)
       (let ([get-pixel (ivar (viewport-DC viewport) get-pixel)])
	 (lambda (posn)
	   (let ([c (make-object wx:colour%)]
		 [x (posn-x posn)]
		 [y (posn-y posn)])
	     (get-pixel x y c)
	     (if (or (< (send c blue) 255)
		     (< (send c red) 255)
		     (< (send c green) 255))
		 1
		 0))))))

   (define draw-pixmap (pixmap-functions 'draw))
   (define clear-pixmap (pixmap-functions 'clear))
   (define flip-pixmap (pixmap-functions 'flip))

   (define DEFAULT-PEN black-pen)
   (define DEFAULT-FONT
     (make-object wx:font% 13 wx:const-roman
		  wx:const-normal wx:const-normal))

   (define copy-viewport 
     (lambda (source target)
       (let*
	   ([source-DC (ivar source DC)]
	    [target-DC (ivar target DC)])
	 (send target-DC blit 0 0 
	       (viewport-width source)
	       (viewport-height source)
	       source-DC 0 0 wx:const-copy))))

   (define make-open-viewport
     (lambda (name show?)
       (letrec ([open-viewport
		 (case-lambda
		  [(label point) 
		   (cond
		    [(posn? point) (open-viewport label (posn-x point) (posn-y point))]
		    [(and (list? point) (= (length point) 2))
		     (open-viewport label (car point) (cadr point))]
		    [else (error name "bad argument ~s" point)])]
		  [(label width height) (open-viewport label width height 1.0)]
		  [(label width height scale)
		   (cond
		    [graphics-flag
		     (let*
			 ([frame
			   (make-object wx:frame% '() label 1 1 
					(* scale width) (* scale height))]
			  [canvas
			   (make-object wx:sixlib-canvas%
					frame 0 0 
					(* scale width) (* scale height)
					wx:const-retained "canvas")]
			  [DC (send canvas get-dc)]
			  [buffer-DC (make-object wx:memory-dc% DC)]
			  [viewport (make-viewport label canvas)])
		       (send canvas set-DC DC)
		       (send canvas set-buffer-DC buffer-DC)
		       (send DC set-optimization #f)
		       (send buffer-DC set-optimization #f)
		       (send canvas set-geometry width height scale)
		       (if show? (send frame show #t))
		       (set-text-foreground viewport black)
		       (set-text-background viewport white)
		       (set-viewport-background viewport white-brush)
		       (set-viewport-pen viewport black-pen)
		       (set-viewport-brush viewport black-brush)
		       ((clear-viewport viewport))
		       (set! GLOBAL-VIEWPORT-LIST (cons viewport GLOBAL-VIEWPORT-LIST))
		       viewport)]
		    [else (error "graphics not open")])])])
	 open-viewport)))

   (define open-viewport (make-open-viewport 'open-viewport #t))
   (define open-pixmap (make-open-viewport 'open-pixmap #f))

   (define default-display-is-color? wx:colour-display?)

   (define position-display
     (lambda (viewport counter)
       (cond
	[(eq? counter 0) '()]
	[else (begin 
		(display (query-mouse-posn viewport))
		(position-display viewport (- counter 1)))])))


   (define create-cmap
     (lambda ()
       (do
	   ([index 0 (+ 1 index)])
	   ((> index 20))
	 (let* ([R (* 0.05 index)]
		[B (- 1 R)]
		[G (- 1 R)])
	   (change-color index (make-rgb R G B))))))

   (define viewport-snipclass
     (make-object
      (class wx:snip-class% ()
	     (inherit set-classname)
	     (sequence
	       (super-init)
	       (set-classname "viewport-snip"))
	     (public
	      [complained? #f]
	      [write-header
	       (lambda ()
		 (set! complained? #f)
		 #t)]
	      [write-complain
	       (lambda ()
		 (unless complained?
			 (set! complained? #t)
			 (wx:message-box "Can't save viewport images" "Warning")))]))))
   
   (define viewport-snip%
     (class wx:snip% (mem-dc bitmap)
	    (inherit set-snipclass)
	    (sequence 
	      (super-init)
	      (set-snipclass viewport-snipclass))
	    (public
	     [get-extent
	      (lambda (dc x y w h descent space lspace rspace)
		(let ([zero! (lambda (x) (unless (null? x)
						 (set-box! x 0.0)))])
		  (unless (null? w)
			  (set-box! w (send bitmap get-width)))
		  (unless (null? h)
			  (set-box! h (send bitmap get-height)))
		  (zero! descent)
		  (zero! space)
		  (zero! lspace)
		  (zero! rspace)))]
	     [draw
	      (lambda (dc x y left top right bottom dx dy draw-caret)
		(let ([w (send bitmap get-width)]
		      [h (send bitmap get-height)])
		  (send dc blit x y w h mem-dc 0 0)))]
	     [copy
	      (lambda ()
		(make-object viewport-snip% mem-dc bitmap))]
	     [write
	      (lambda ()
		(send viewport-snipclass write-complain))])))

   (define viewport->snip
     (lambda (viewport)
       (let* ([dc (viewport-buffer-DC viewport)]
	      [bitmap (ivar (viewport-canvas viewport) bitmap)]
	      [snip (make-object viewport-snip% dc bitmap)])
	 snip)))
	 
   (create-cmap)))
