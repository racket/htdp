; graphics.ss
; Simple graphics routines for MrEd
; Originally written by Johnathan Franklin

(unit/sig graphics^
  (import mzlib:file^
	  (mred : mred^))
  
  (define-struct viewport (label canvas))
  (define-struct posn (x y))
  (define-struct sixmouse (x y left? middle? right?))
  (define-struct sixkey (value))
  (define graphics-flag #f)
  (define global-viewport-list '())
  (define global-color-vector (make-vector 300))
  (define global-pen-vector (make-vector 300))
  (define global-brush-vector (make-vector 300))
  (define default-font (make-object mred:font% 12 'roman 'normal 'normal))
  
  (define sixlib-canvas%
    (class-asi mred:canvas% 
      (inherit get-parent
	       min-client-width min-client-height
	       stretchable-width stretchable-height)
      (private
	[current-mouse-posn (make-posn 0 0)]
	[queue%
	 (class object% ()
	   (private
	     [queue '()]
	     [last #f]
	     [lock (make-semaphore 1)]
	     [ready (make-semaphore)])
	   (public
	     [flush
	      (lambda ()
		(semaphore-wait lock)
		(set! queue '())
		(set! last #f)
		(set! ready (make-semaphore))
		(semaphore-post lock))]
	     [add
	      (lambda (v)
		(semaphore-wait lock)
		(if last
		    (begin
		      (set-cdr! last (cons v '()))
		      (set! last (cdr last)))
		    (begin
		      (set! queue (cons v '()))
		      (set! last queue)))
		(semaphore-post ready)
		(semaphore-post lock))]
	     [remove
	      (lambda ()
		(semaphore-wait lock)
		(begin0
		 (if (null? queue)
		     #f
		     (begin0
		      (car queue)
		      (semaphore-wait ready)
		      (set! queue (cdr queue))
		      (if (null? queue)
			  (set! last #f))))
		 (semaphore-post lock)))]
	     [remove/wait
	      (lambda ()
		(semaphore-wait ready)
		(semaphore-post ready)
		(remove))])
	   (sequence
	     (super-init)))]
	[click-queue (make-object queue%)]
	[release-queue (make-object queue%)]
	[press-queue (make-object queue%)]
	[reset-size
	 (lambda ()
	   (let ([width (inexact->exact (floor (* scale width)))]
		 [height (inexact->exact (floor (* scale height)))])
	     (min-client-width width)
	     (min-client-height height)
	     (stretchable-width #f)
	     (stretchable-height #f)
	     (set! bitmap (make-object mred:bitmap% width height))
	     (unless (send bitmap ok?)
	       (error "cannot allocate viewport"))
	     (send buffer-dc set-bitmap bitmap)
	     (send buffer-dc set-brush (send dc get-brush))
	     (send buffer-dc set-pen (send dc get-pen))
	     (let ([f (send dc get-font)])
	       (when f
		 (send buffer-dc set-font f)))
	     (send buffer-dc clear)
	     (send dc clear)))])
      
      
      (public
	viewport
	[set-viewport (lambda (x) (set! viewport x))]
	[scale 1.0]
	[height 0]
	[width 0]
	[label 0]
	[current-pen 'uninitialized-pen]
	[current-brush 'uninitialized-brush]
	[bitmap 'uninitalized-bitmap]
	[dc 'uninitialized-dc]
	[buffer-dc 'uninitialized-buffer-dc]
	[get-current-pen (lambda () current-pen)]
	[get-current-brush (lambda () current-brush)]
	[remember-pen (lambda (pen) (set! current-pen pen))]
	[remember-brush (lambda (brush) (set! current-brush brush))])
      
      (override
       [on-paint
	(lambda ()
	  (let ([bm (send buffer-dc get-bitmap)])
	    (send dc draw-bitmap bm 0 0)))]
       
       [on-event 
	(lambda (mouse-event)
	  (let* ([x (send mouse-event get-x)]
		 [y (send mouse-event get-y)]
		 [left? (send mouse-event button-down? 'left)]
		 [middle? (send mouse-event button-down? 'middle)]
		 [right? (send mouse-event button-down? 'right)]
		 [sixm (make-sixmouse x y left? middle? right?)])
	    (set! current-mouse-posn (make-posn x y))
	    (cond
	      [(send mouse-event button-down?)
	       (send click-queue add sixm)]
	      [(send mouse-event button-up?)
	       (send release-queue add sixm)]
	      [else (void)])))]
       
       [on-char
	(lambda (key-event)
	  (send press-queue add (make-sixkey (send key-event get-key-code))))])
      
      (public
	[get-click
	 (lambda ()
	   (send click-queue remove))]
	[get-click-now
	 (lambda ()
	   (send click-queue remove/wait))]
	
	[get-release
	 (lambda ()
	   (send release-queue remove))]
	[get-release-noew
	 (lambda ()
	   (send release-queue remove/wait))]
	
	[get-press
	 (lambda ()
	   (send press-queue remove))]
	[get-press-now
	 (lambda ()
	   (send press-queue remove/wait))]
	
	[get-posn (lambda () current-mouse-posn)]
	[set-dc (lambda (new-dc) (set! dc new-dc))]
	[set-buffer-dc (lambda (new-buffer-dc) (set! buffer-dc
						     new-buffer-dc))]
	
	[set-geometry
	 (lambda (new-width new-height new-scale)
	   (set! height new-height)
	   (set! width new-width)
	   (set! scale new-scale)
	   (reset-size))]
	[set-height (lambda (new-height) 
		      (set! height new-height)
		      (reset-size))]
	[set-width (lambda (new-width) 
		     (set! width new-width)
		     (reset-size))]
	[set-scale (lambda (new-scale)
		     (set! scale new-scale)
		     (send dc set-scale scale scale)
		     (send buffer-dc set-scale scale scale)
		     (reset-size))]
	
	
	[viewport-flush-input
	 (lambda ()
	   (send click-queue flush)
	   (send release-queue flush)
	   (send press-queue flush))])))
  
  (define sixlib-frame%
    (class-asi mred:frame%
      (rename [super-on-close on-close])
      (public
	[canvas #f]
	[set-canvas (lambda (x) (set! canvas x))])
      (override
       [on-close
	(lambda ()
	  (close-viewport (ivar canvas viewport))
	  (super-on-close))])))
  
  (define repaint
    (lambda (viewport)
      (send (viewport-canvas viewport) on-paint)))
  
  (define viewport-dc 
    (lambda (viewport)
      (ivar (viewport-canvas viewport) dc)))
  
  (define viewport-buffer-dc
    (lambda (viewport)
      (ivar (viewport-canvas viewport) buffer-dc)))

  (define viewport-bitmap
    (lambda (viewport)
      (ivar (viewport-canvas viewport) bitmap)))
  
  (define viewport-frame
    (lambda (viewport)
      (send (send (viewport-canvas viewport) get-parent) get-parent)))
  
  (define viewport-height
    (lambda (viewport)
      (ivar (viewport-canvas viewport) height)))
  
  (define viewport-width
    (lambda (viewport)
      (ivar (viewport-canvas viewport) width)))
  
  (define (get-mouse-click viewport)
    (send (viewport-canvas viewport) get-click-now))
  
  (define (get-key-press viewport) 
    (send (viewport-canvas viewport) get-press-now))
  
  (define (ready-mouse-click viewport)
    (send (viewport-canvas viewport) get-click))
  
  (define (ready-mouse-release viewport)
    (send (viewport-canvas viewport) get-release))
  
  (define (ready-key-press viewport)
    (send (viewport-canvas viewport) get-press))
  
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
      (let* ([clear (ivar (viewport-dc viewport) clear)]
	     [clear2 (ivar (viewport-buffer-dc viewport) clear)])
	(lambda ()
	  (clear)
	  (clear2)))))
  
  (define draw-viewport
    (lambda (viewport)
      (let* ([dc (viewport-dc viewport)]
	     [buffer-dc (viewport-buffer-dc viewport)]
	     [draw (ivar dc draw-rectangle)]
	     [draw2 (ivar buffer-dc draw-rectangle)]
	     [w (viewport-width viewport)]
	     [h (viewport-height viewport)])
	(rec draw-viewport/color
	     (case-lambda
	      [(color)
	       (let ([new-pen (send mred:the-pen-list find-or-create-pen color 1 'solid)]
		     [new-brush (send mred:the-brush-list find-or-create-brush color 'solid)]
		     [old-pen (send dc get-pen)]
		     [old-brush (send dc get-brush)])
		 (send dc set-pen new-pen)
		 (send dc set-brush new-brush)
		 (send buffer-dc set-pen new-pen)
		 (send buffer-dc set-brush new-brush)
		 (draw 0 0 w h)
		 (draw2 0 0 w h)
		 (send dc set-pen old-pen)
		 (send buffer-dc set-pen old-pen)
		 (send dc set-brush old-brush)
		 (send buffer-dc set-brush old-brush))]
	      [() (draw-viewport/color (make-rgb 0 0 0))])))))
  
  (define flip-viewport
    (lambda (viewport)
      (let* ([dc (viewport-dc viewport)]
	     [dc2 (viewport-buffer-dc viewport)]
	     [set-pen (ivar dc set-pen)]
	     [set-pen2 (ivar dc2 set-pen)]
	     [set-brush (ivar dc set-brush)]
	     [set-brush2 (ivar dc2 set-brush)]
	     [draw (ivar dc draw-rectangle)]
	     [draw2 (ivar dc2 draw-rectangle)]
	     [w (viewport-width viewport)]
	     [h (viewport-height viewport)])
	(lambda ()
	  (let ([pen (send dc get-pen)]
		[pen2 (send dc2 get-pen)]
		[brush (send dc get-brush)]
		[brush2 (send dc2 get-brush)])
	    (set-pen xor-pen)
	    (set-pen2 xor-pen)
	    (set-brush xor-brush)
	    (set-brush2 xor-brush)
	    (draw 0 0 w h)
	    (draw2 0 0 w h)
	    (set-pen pen)
	    (set-pen2 pen2)
	    (set-brush brush)
	    (set-brush2 brush2))))))
  
  (define close-viewport
    (lambda (viewport)
      (set! global-viewport-list 
	    (let loop ([l global-viewport-list])
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
      (map close-viewport global-viewport-list)
      (set! graphics-flag #f)
      (set! global-viewport-list '())))
  
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
	(make-object mred:color% nred ngreen nblue))))
  
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
  
  (define rgb-blue (lambda (rgb) (caddr (get-rgb-map rgb))))
  (define rgb-red (lambda (rgb) (car (get-rgb-map rgb))))
  (define rgb-green (lambda (rgb) (cadr (get-rgb-map rgb))))
  
  (define change-color
    (lambda (index color)
      (vector-set! global-color-vector index color)
      (vector-set! global-pen-vector index (get-pen color))
      (vector-set! global-brush-vector index (get-brush color))))
  
  (define get-color
    (lambda (index)
      (cond
	[(is-a? index mred:color%) index]
	[(string? index) (make-object mred:color% index)]
	[else (vector-ref global-color-vector index)])))
  
  (define get-pen
    (lambda (index)
      (cond
	[(is-a? index mred:pen%) index]
	[(or (string? index) (is-a? index mred:color%))
	 (send mred:the-pen-list find-or-create-pen index 1 'solid)]
	[else (vector-ref global-pen-vector index)])))
  
  (define get-brush
    (lambda (index)
      (cond
	[(is-a? index mred:brush%) index]
	[(or (string? index) (is-a? index mred:color%))
	 (send mred:the-brush-list find-or-create-brush index 'solid)]
	[else (vector-ref global-brush-vector index)])))
  
  (define rgb? (lambda (object) (is-a? object mred:color%)))
  (define color? rgb?)
  (define pen? (lambda (object) (is-a? object mred:pen%)))
  (define brush? (lambda (object) (is-a? object mred:brush%)))
  
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
	 (make-object mred:font% 40 'decorative 'normal 'normal)]
	[(eq? name 'small-roman)
	 (make-object mred:font% 12 'roman 'normal 'normal)]
	[(eq? name 'medium-roman)
	 (make-object mred:font% 24 'roman 'normal 'normal)]
	[(eq? name 'large-roman)
	 (make-object mred:font% 32 'roman 'normal 'normal)]
	[else "no such font ~a; only 'large-deco, 'small-roman, 'medium-roman, and 'large-roman"
	      name])))
  
  (define custom-roman
    (lambda (size)
      (make-object mred:font%
		   size 'roman 'normal 'normal)))
  
  (define custom-deco
    (lambda (size)
      (make-object mred:font% size 'decorative 'normal 'normal)))
  
  (define set-viewport-pen
    (lambda (viewport pen)
      (send (viewport-canvas viewport) remember-pen pen)
      (let ([pen (get-pen pen)])
	(send (viewport-dc viewport) set-pen pen)
	(send (viewport-buffer-dc viewport) set-pen pen))))
  
  (define set-viewport-brush
    (lambda (viewport brush)
      (send (viewport-canvas viewport) remember-brush brush)
      (let ([brush (get-brush brush)])
	(send (viewport-dc viewport) set-brush brush)
	(send (viewport-buffer-dc viewport) set-brush brush))))
  
  (define set-text-foreground
    (lambda (viewport color)
      (let ([color (get-color color)])
	(send (viewport-dc viewport) set-text-foreground color)
	(send (viewport-buffer-dc viewport) set-text-foreground color))))
  
  (define set-text-background
    (lambda (viewport color)
      (let ([color (get-color color)])
	(send (viewport-dc viewport) set-text-background color)
	(send (viewport-buffer-dc viewport) set-text-background color))))
  
  (define set-viewport-font
    (lambda (viewport font)
      (send (viewport-dc viewport) set-font font)
      (send (viewport-buffer-dc viewport) set-font font)))
  
  (define set-viewport-background
    (lambda (viewport color)
      (send (viewport-dc viewport) set-background color)
      (send (viewport-buffer-dc viewport) set-background color)))
  
  (define set-viewport-logical-function
    (lambda (viewport logical-function)
      (send (viewport-dc viewport) set-logical-function logical-function)
      (send (viewport-buffer-dc viewport) set-logical-function
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
  
  (define invisi-pen (send mred:the-pen-list find-or-create-pen "WHITE" 0 'transparent))
  (define invisi-brush (send mred:the-brush-list find-or-create-brush "WHITE" 'transparent))
  
  (define xor-pen (send mred:the-pen-list find-or-create-pen "BLACK" 1 'xor))
  (define xor-brush (send mred:the-brush-list find-or-create-brush "BLACK" 'xor))
  
  (define draw-it (lambda (draw flip clear) (draw)))
  (define flip-it (lambda (draw flip clear) (flip)))
  (define clear-it (lambda (draw flip clear) (clear)))
  
  (define make-draw-proc
    (lambda (draw-name get-pen-name set-pen-name 
		       get-current-pen-name set-viewport-pen white-pen)
      (lambda (viewport) 
	(let* ([current-pen (ivar/proc (viewport-canvas viewport) get-current-pen-name)]
	       [draw-1 (ivar/proc (viewport-dc viewport) draw-name)]
	       [draw-2 (ivar/proc (viewport-buffer-dc viewport) draw-name)]
	       [get-pen (ivar/proc (viewport-dc viewport) get-pen-name)]
	       [real-get-brush (ivar/proc (viewport-dc viewport) 'get-brush)]
	       [real-get-pen (ivar/proc (viewport-dc viewport) 'get-pen)]
	       [set-pen-1 (ivar/proc (viewport-dc viewport) set-pen-name)]
	       [set-pen-2 (ivar/proc (viewport-buffer-dc viewport) set-pen-name)]
	       [real-set-brush-1 (ivar/proc (viewport-dc viewport) 'set-brush)]
	       [real-set-brush-2 (ivar/proc (viewport-buffer-dc viewport) 'set-brush)]
	       [real-set-pen-1 (ivar/proc (viewport-dc viewport) 'set-pen)]
	       [real-set-pen-2 (ivar/proc (viewport-buffer-dc viewport) 'set-pen)])
	  (lambda (color go)
	    (let ([orig (and color
			     (begin0
			      (current-pen)
			      (set-viewport-pen viewport (get-color color))))])
	      (go draw-1 draw-2
		  (lambda (draw)
		    (let ([pen (real-get-pen)]
			  [brush (real-get-brush)])
		      (real-set-brush-1 xor-brush)
		      (real-set-brush-2 xor-brush)
		      (real-set-pen-1 xor-pen)
		      (real-set-pen-2 xor-pen)
		      (draw)
		      (real-set-brush-1 brush)
		      (real-set-brush-2 brush)
		      (real-set-pen-1 pen)
		      (real-set-pen-2 pen)))
		  (lambda (draw)
		    (let ([pen (get-pen)])
		      (set-pen-1 white-pen)
		      (set-pen-2 white-pen)
		      (draw)
		      (set-pen-1 pen)
		      (set-pen-2 pen))))
	      (when orig
		(set-viewport-pen viewport orig))))))))
  
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
  (define (clear-line viewport)
    (let ([f ((make-do-line clear-it) viewport)])
      (rec clear-line-viewport
	   (lambda (p1 p2)
	     (f p1 p2)))))
  (define (flip-line viewport)
    (let ([f ((make-do-line flip-it) viewport)])
      (rec flip-line-viewport
	   (lambda (p1 p2)
	     (f p1 p2)))))
  
  (define (draw/clear/flip ivar)
    (lambda (init-dc viewport p width height)
      (let ([dc (viewport-dc viewport)]
	    [buffer-dc (viewport-buffer-dc viewport)])
	(init-dc dc)
	(init-dc buffer-dc)
	((ivar/proc dc ivar) (posn-x p) (posn-y p) width height)
	((ivar/proc buffer-dc ivar) (posn-x p) (posn-y p) width height))))
  
  (define draw/clear/flip-rectangle (draw/clear/flip 'draw-rectangle))
  (define draw/clear/flip-ellipse (draw/clear/flip 'draw-ellipse))
  
  (define (draw-rectangle viewport)
    (rec draw-rectangle-viewport
	 (case-lambda 
	  [(p width height) (draw-rectangle-viewport p width height "BLACK")]
	  [(p width height color)
	   (draw/clear/flip-rectangle
	    (lambda (dc)
	      (send dc set-pen (send mred:the-pen-list find-or-create-pen color 1 'solid))
	      (send dc set-brush (send mred:the-brush-list find-or-create-brush "BLACK" 'transparent)))
	    viewport p width height)])))
    
  (define (draw-solid-rectangle viewport)
    (rec draw-solid-rectangle-viewport
	 (case-lambda 
	  [(p width height) (draw-solid-rectangle-viewport p width height "BLACK")]
	  [(p width height color)
	   (draw/clear/flip-rectangle
	    (lambda (dc)
	      (send dc set-pen (send mred:the-pen-list find-or-create-pen color 1 'solid))
	      (send dc set-brush (send mred:the-brush-list find-or-create-brush color 'solid)))
	    viewport p width height)])))
  
  (define (draw-ellipse viewport)
    (rec draw-ellipse-viewport
	 (case-lambda 
	  [(p width height) (draw-ellipse-viewport p width height "BLACK")]
	  [(p width height color)
	   (draw/clear/flip-ellipse
	    (lambda (dc)
	      (send dc set-pen (send mred:the-pen-list find-or-create-pen color 1 'solid))
	      (send dc set-brush (send mred:the-brush-list find-or-create-brush "BLACK" 'transparent)))
	    viewport p width height)])))
  
  (define (draw-solid-ellipse viewport)
    (rec draw-solid-ellipse-viewport
	 (case-lambda 
	  [(p width height) (draw-solid-ellipse-viewport p width height "BLACK")]
	  [(p width height color)
	   (draw/clear/flip-ellipse
	    (lambda (dc)
	      (send dc set-pen (send mred:the-pen-list find-or-create-pen color 1 'solid))
	      (send dc set-brush (send mred:the-brush-list find-or-create-brush color 'solid)))
	    viewport p width height)])))
  
  (define (flip-rectangle viewport)
    (rec flip-rectangle-viewport
	 (case-lambda 
	  [(p width height) (flip-rectangle-viewport p width height "BLACK")]
	  [(p width height color)
	   (draw/clear/flip-rectangle
	    (lambda (dc)
	      (send dc set-pen (send mred:the-pen-list find-or-create-pen color 1 'xor))
	      (send dc set-brush (send mred:the-brush-list find-or-create-brush "BLACK" 'transparent)))
	    viewport p width height)])))
  
  (define (flip-solid-rectangle viewport)
    (rec flip-solid-rectangle-viewport
	 (case-lambda 
	  [(p width height) (flip-solid-rectangle-viewport p width height "BLACK")]
	  [(p width height color)
	   (draw/clear/flip-rectangle
	    (lambda (dc)
	      (send dc set-pen (send mred:the-pen-list find-or-create-pen "BLACK" 1 'transparent))
	      (send dc set-brush (send mred:the-brush-list find-or-create-brush color 'xor)))
	    viewport p width height)])))
  
  (define (flip-ellipse viewport)
    (rec flip-ellipse-viewport
	 (case-lambda 
	  [(p width height) (flip-ellipse-viewport p width height "BLACK")]
	  [(p width height color)
	   (draw/clear/flip-ellipse
	    (lambda (dc)
	      (send dc set-pen (send mred:the-pen-list find-or-create-pen color 1 'xor))
	      (send dc set-brush (send mred:the-brush-list find-or-create-brush "BLACK" 'transparent)))
	    viewport p width height)])))
  
  (define (flip-solid-ellipse viewport)
    (rec flip-solid-ellipse-viewport
	 (case-lambda 
	  [(p width height) (flip-solid-ellipse-viewport p width height "BLACK")]
	  [(p width height color)
	   (draw/clear/flip-ellipse
	    (lambda (dc)
	      (send dc set-pen (send mred:the-pen-list find-or-create-pen "BLACK" 1 'transparent))
	      (send dc set-brush (send mred:the-brush-list find-or-create-brush color 'xor)))
	    viewport p width height)])))
  
  (define (clear-rectangle viewport)
    (rec clear-rectangle-viewport
	 (lambda (p width height)
	   (draw/clear/flip-rectangle
	    (lambda (dc)
	      (send dc set-pen (send mred:the-pen-list find-or-create-pen "WHITE" 1 'solid))
	      (send dc set-brush (send mred:the-brush-list find-or-create-brush "BLACK" 'transparent)))
	    viewport p width height))))
  
  (define (clear-solid-rectangle viewport)
    (rec clear-solid-rectangle-viewport
	 (lambda (p width height)
	   (draw/clear/flip-rectangle
	    (lambda (dc)
	      (send dc set-pen (send mred:the-pen-list find-or-create-pen "WHITE" 1 'solid))
	      (send dc set-brush (send mred:the-brush-list find-or-create-brush "WHITE" 'solid)))
	    viewport p width height))))
  
  (define (clear-ellipse viewport)
    (rec clear-ellipse-viewport
	 (lambda (p width height)
	   (draw/clear/flip-ellipse
	    (lambda (dc)
	      (send dc set-pen (send mred:the-pen-list find-or-create-pen "WHITE" 1 'solid))
	      (send dc set-brush (send mred:the-brush-list find-or-create-brush "BLACK" 'transparent)))
	    viewport p width height))))
  
  (define (clear-solid-ellipse viewport)
    (rec clear-solid-ellipse-viewport
	 (lambda (p width height)
	   (draw/clear/flip-ellipse
	    (lambda (dc)
	      (send dc set-pen (send mred:the-pen-list find-or-create-pen "WHITE" 1 'solid))
	      (send dc set-brush (send mred:the-brush-list find-or-create-brush "WHITE" 'solid)))
	    viewport p width height))))

  (define make-do-pointlist
    (lambda (go name get-pen-name set-pen-name
		get-current-pen-name set-viewport-pen white-pen
		get-brush-name set-brush-name invisi-brush)
      (let ([f (make-draw-proc name get-pen-name set-pen-name 
			       get-current-pen-name set-viewport-pen white-pen)])
	(lambda (viewport)
	  (let ([f (f viewport)]
		[get-brush (ivar/proc (viewport-dc viewport) get-brush-name)]
		[set-brush-1 (ivar/proc (viewport-dc viewport) set-brush-name)]
		[set-brush-2 (ivar/proc (viewport-buffer-dc viewport) set-brush-name)])
	    (letrec ([the-function
		      (case-lambda
		       [(posns offset) (the-function posns offset #f)]
		       [(posns offset color)
			(f color
			   (lambda (draw-1 draw-2 flip clear)
			     (let* ([points (map (lambda (p)
						   (make-object mred:point% (posn-x p) (posn-y p)))
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
  (define (clear-polygon viewport)
    (let ([f ((make-do-polygon clear-it) viewport)])
      (rec clear-polygon-viewport
	   (lambda (posns offset)
	     (f posns offset)))))
  (define (flip-polygon viewport)
    (let ([f ((make-do-polygon flip-it) viewport)])
      (rec flip-polygon-viewport
	   (lambda (posns offset)
	     (f posns offset)))))
  
  (define draw-solid-polygon (make-do-solid-polygon draw-it))
  (define (clear-solid-polygon viewport)
    (let ([f ((make-do-solid-polygon clear-it) viewport)])
      (rec clear-solid-polygon-viewport
	   (lambda (posns offset)
	     (f posns offset)))))
  (define (flip-solid-polygon viewport)
    (let ([f ((make-do-solid-polygon flip-it) viewport)])
      (rec flip-solid-polygon-viewport
	   (lambda (posns offset)
	     (f posns offset)))))
  
  (define make-do-pixel
    (lambda (go)
      (let ([f (make-draw-proc 'draw-point 'get-pen 'set-pen  
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
  (define (clear-pixel viewport)
    (let ([f ((make-do-pixel clear-it) viewport)])
      (rec clear-pixel-viewport
	   (lambda (posns offset)
	     (f posns offset)))))
  (define (flip-pixel viewport)
    (let ([f ((make-do-pixel flip-it) viewport)])
      (rec flip-pixel-viewport
	   (lambda (posns offset)
	     (f posns offset)))))
  
  (define string-functions
    (lambda (string-op)
      (letrec ([outer-function
		(case-lambda
		 [(viewport) (outer-function viewport default-font)]
		 [(viewport font)
		  (letrec ([the-function
			    (case-lambda
			     [(posn text) (the-function posn text #f)]
			     [(posn text color)
			      (let*-values ([(dc) (viewport-dc viewport)]
					    [(x) (posn-x posn)]
					    [(w h d a) (send dc get-text-extent "X" font)]
					    [(y) (- (posn-y posn) (- h d))]
					    [(buffer) (viewport-buffer-dc viewport)]
					    [(string-create)
					     (lambda ()
					       (send dc draw-text text x y)
					       (send buffer draw-text text x y))])
				(cond
				  [(eq? string-op 'draw)
				   (when color
				     (set-text-foreground viewport color))
				   (set-viewport-font viewport font)
				   (send dc draw-text text x y)
				   (send buffer draw-text text x y)]
				  [(eq? string-op 'flip)
				   (when color
				     (set-text-foreground viewport color))
				   (set-viewport-font viewport font)
				   (string-create)]
				  [(eq? string-op 'clear)
				   (set-text-foreground viewport white)
				   (set-viewport-font viewport font)
				   (string-create)
				   (set-text-foreground viewport black)]))])])
		    the-function)])])
	outer-function)))
  
  (define draw-string (string-functions 'draw))
  (define (clear-string viewport)
    (let ([f ((string-functions 'clear) viewport)])
      (rec clear-string-viewport
	   (lambda (posns offset)
	     (f posns offset)))))
  (define (flip-string viewport)
    (let ([f ((string-functions 'flip) viewport)])
      (rec flip-string-viewport
	   (lambda (posns offset)
	     (f posns offset)))))
  
  (define get-string-size
    (case-lambda
     [(viewport) (get-string-size viewport default-font)]
     [(viewport font)
      (let ([get-extent (ivar (viewport-dc viewport) get-text-extent)])
	(lambda (text)
	  (let-values ([(w h d a) (get-extent text font)])
	    (list w h))))]))
  
  (define get-color-pixel 
    (lambda (viewport)
      (let ([get-pixel (ivar (viewport-buffer-dc viewport) get-pixel)])
	(lambda (posn)
	  (let ([c (make-object mred:color%)]
		[x (posn-x posn)]
		[y (posn-y posn)])
	    (unless (get-pixel x y c)
	      (error 'get-color-pixel "specified point is out-of-range"))
	    c)))))
  
  (define get-pixel 
    (lambda (viewport)
      (let ([get-pixel (ivar (viewport-buffer-dc viewport) get-pixel)])
	(lambda (posn)
	  (let ([c (make-object mred:color%)]
		[x (posn-x posn)]
		[y (posn-y posn)])
	    (unless (get-pixel x y c)
	      (error 'get-pixel "specified point is out-of-range"))
	    (if (or (< (send c blue) 255)
		    (< (send c red) 255)
		    (< (send c green) 255))
		1
		0))))))
  
  (define draw-pixmap-posn
    (opt-lambda (filename [type 'unknown])
      (let* ([type
	      (case type
		[(gif xbm xpm bmp pict unknown) type]
		[else 
		 (error 'pixmap "unrecognized file type: ~e~n" type)])]
	     [bitmap (make-object mred:bitmap% filename type)])
	(lambda (viewport)
	  (opt-lambda (posn [color #f])
	    (when color
	      (set-viewport-pen viewport (get-color color)))
	    (let ([x (posn-x posn)]
		  [y (posn-y posn)])
	      (send (viewport-dc viewport) draw-bitmap bitmap x y)
	      (send (viewport-buffer-dc viewport) draw-bitmap bitmap x y)))))))
  
  (define draw-pixmap
    (lambda (pixmap)
      (opt-lambda (filename p [color #f])
	(((draw-pixmap-posn filename 'unknown) pixmap) p color))))
  
  (define copy-viewport 
    (lambda (source target)
      (let* ([source-bitmap (viewport-bitmap source)]
	     [target-dc (viewport-dc target)]
	     [target-buffer-dc (viewport-buffer-dc target)])
	(send target-dc draw-bitmap source-bitmap 0 0)
	(send target-buffer-dc draw-bitmap source-bitmap 0 0))))
  
  (define sixlib-eventspace #f)
  
  (define make-open-viewport
    (lambda (name show?)
      (unless sixlib-eventspace
	(set! sixlib-eventspace 
	      (parameterize ([current-exception-handler
			      (lambda (x)
				((error-display-handler)
				 (format "internal error in graphics library: ~a"
					 (if (exn? x)
					     (exn-message x)
					     (format "~e" x))))
				((error-escape-handler)))])
		(mred:make-eventspace))))
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
			   (parameterize ([mred:current-eventspace sixlib-eventspace])
			     (make-object sixlib-frame%
					  label #f
					  (inexact->exact (floor (* scale width)))
					  (inexact->exact (floor (* scale height)))))]
			  [panel (make-object mred:vertical-panel% frame)]
			  [canvas (make-object sixlib-canvas% panel)]
			  [_ (begin
			       (send canvas min-height (inexact->exact (floor (* scale height))))
			       (send canvas min-width (inexact->exact (floor (* scale width)))))]
			  [dc (send canvas get-dc)]
			  [buffer-dc (make-object mred:bitmap-dc%)]
			  [viewport (make-viewport label canvas)])
		       (send panel set-alignment 'center 'center)
		       (send frame set-canvas canvas)
		       (send canvas set-viewport viewport)
		       (send canvas set-dc dc)
		       (send canvas set-buffer-dc buffer-dc)
		       (send canvas set-geometry width height scale)
		       (when show? 
			 (send frame show #t)
			 (send canvas focus))
		       (set-text-foreground viewport black)
		       (set-text-background viewport white)
		       (set-viewport-background viewport white)
		       (set-viewport-pen viewport black-pen)
		       (set-viewport-brush viewport black-brush)
		       ((clear-viewport viewport))
		       (set! global-viewport-list (cons viewport global-viewport-list))
		       viewport)]
		    [else (error "graphics not open")])])])
	open-viewport)))
  
  (define open-viewport (make-open-viewport 'open-viewport #t))
  (define open-pixmap (make-open-viewport 'open-pixmap #f))
  
  (define (default-display-is-color?) (mred:is-color-display?))
  
  (define position-display
    (lambda (viewport counter)
      (cond
	[(equal? counter 0) '()]
	[else (begin 
		(display (query-mouse-posn viewport))
		(position-display viewport (- counter 1)))])))
  
  
  (define create-cmap
    (lambda ()
      (do ([index 0 (+ 1 index)])
	((> index 20))
	(let* ([r (* 0.05 index)]
	       [b (- 1 r)]
	       [g (- 1 r)])
	  (change-color index (make-rgb r g b))))))
  
  (define viewport->snip
    (lambda (viewport)
      (let ([orig-bitmap (ivar (viewport-canvas viewport) bitmap)]
	    [orig-dc (viewport-buffer-dc viewport)])
	(let* ([h (send orig-bitmap get-height)]
	       [w (send orig-bitmap get-width)]
	       [new-bitmap (make-object mred:bitmap% w h)]
	       [tmp-mem-dc (make-object mred:bitmap-dc%)])
	  (send tmp-mem-dc set-bitmap new-bitmap)
	  (send tmp-mem-dc draw-bitmap (send orig-dc get-bitmap) 0 0)
	  (send tmp-mem-dc set-bitmap #f)
	  (let ([snip (make-object mred:image-snip%)])
	    (send snip set-bitmap new-bitmap)
	    snip)))))
  
  (create-cmap))
