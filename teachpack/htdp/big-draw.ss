(require-library "error-sig.ss" "htdp")
(require-library "draw-sig.ss" "htdp")
(require-library "graphics.ss" "graphics")

(define drawU
  (unit/sig coreDrawS
    (import errorS plt:userspace^ graphics^)
  
    (define the-error
      (lambda x
	(error "evaluate (start <num> <num>) first")))  

    (define %draw-solid-disk the-error)
    (define draw-solid-disk (lambda a (apply %draw-solid-disk a)))

    (define %clear-solid-disk the-error)
    (define clear-solid-disk (lambda a (apply %clear-solid-disk a)))

    (define %draw-circle the-error)
    (define draw-circle (lambda a (apply %draw-circle a)))

    (define %clear-circle the-error)
    (define clear-circle (lambda a (apply %clear-circle a)))

    (define %draw-solid-rect the-error)
    (define draw-solid-rect (lambda a (apply %draw-solid-rect a)))

    (define %clear-solid-rect the-error)
    (define clear-solid-rect (lambda a (apply %clear-solid-rect a)))
  
    (define %draw-solid-line the-error) 
    (define draw-solid-line (lambda a (apply %draw-solid-line a)))

    (define %clear-solid-line the-error)
    (define clear-solid-line (lambda a (apply %clear-solid-line a)))

    (define %clear-all the-error)
    (define clear-all (lambda a (apply %clear-all a)))

    (define (make-true f) (lambda x (apply f x) #t))

    (define sleep-for-a-while (make-true sleep))

    ;; i wish i could abstract these functions ...
    (define (make-line name f)
      (make-true
	(lambda x
	  (check-arity name 2 x)
	  (apply (lambda (p1 p2 . c)
		   (check-arg name (posn? p1) "posn" "first" p1)
		   (check-arg name (posn? p2) "posn" "second" p2)
		   (f p1 p2 (if (null? c)
				BLACK
				(let ((c (car c)))
				  (check-arg name (rgb? c) "color" "third" c)
				  c))))
	    x))))

    (define (make-rect name f)
      (make-true
	(lambda x
	  (check-arity name 3 x)
	  (apply (lambda (p w h . c)
		   (check-arg name (posn? p) "posn" "first" p)
		   (check-arg name (and (integer? w) (> w 0)) "positive integer" "second" w)
		   (check-arg name (and (integer? h) (> h 0)) "positive integer" "third" h)
		   (f p w h (if (null? c)
				BLACK
				(let ((c (car c)))
				  (check-arg name (rgb? c) "color" "fourth" c)
				  c))))
	    x))))
	
    (define (make-circle name f)
      (make-true
	(lambda x
	  (check-arity name 2 x)
	  (apply (lambda (p r . c)
		   (check-arg name (posn? p) "posn" "first" p)
		   (check-arg name (and (integer? r) (> r 0)) "positive integer" "second" r)
		   (let ((d (* r 2))
			 (c (if (null? c) BLACK (car c))))
		     (check-arg name (rgb? c) "color" "third" c)
		     (f (make-posn (- (posn-x p) r) (- (posn-y p) r)) d d c)))
	    x))))

    (define (start WIDTH HEIGHT)
      (check-arg 'start (and (integer? WIDTH) (> WIDTH 0)) "positive integer" "first" WIDTH)
      (check-arg 'start (and (integer? HEIGHT) (> HEIGHT 0)) "positive integer" "second" HEIGHT)
      ;; --- 
      (open-graphics)
      (let ((current-window (open-viewport "Canvas" WIDTH HEIGHT)))
	(set! @VP current-window)
	(set! %clear-all (clear-viewport current-window))

	(set! %draw-solid-line (make-line 'draw-solid-line (draw-line current-window)))
	(set! %clear-solid-line
	  (make-line 'clear-solid-line
	    (lambda (p1 p2 c)
	      ((clear-line current-window) p1 p2))))

	(set! %draw-solid-rect (make-rect 'draw-solid-rect (draw-solid-rectangle current-window)))
	(set! %clear-solid-rect
	  (make-rect 'clear-solid-rect
	    (lambda (p w h c)
	      ((clear-solid-rectangle current-window) p w h))))

	(set! %draw-solid-disk (make-circle 'draw-solid-disk (draw-solid-ellipse current-window)))
	(set! %clear-solid-disk
	  (make-circle 'clear-solid-disk
	    (lambda (p r1 r2 c)
	      ((clear-solid-ellipse current-window) p r1 r2))))

	(set! %draw-circle (make-circle 'draw-circle (draw-ellipse current-window)))
	(set! %clear-circle
	  (make-circle 'clear-circle
	    (lambda (p r1 r2 c)
	      ((clear-ellipse current-window) p r1 r2))))))
  
    (define (stop)
      (close-graphics))
  
    (define @VP #f)

    (define WHITE (make-rgb 1 1 1))
    (define YELLOW (make-rgb 1 1 0))
    (define RED (make-rgb 1.0 0 0))
    (define GREEN (make-rgb 0 1.0 0))
    (define BLUE (make-rgb 0 0 1.0))
    (define BLACK (make-rgb 0 0 0))
  
    ))

(define-signature bigDrawS
  ((open coreDrawS)
   (open graphics^)))

(define bigDrawU
  (compound-unit/sig
   (import (ERR : errorS) (PLT : plt:userspace^))
   (link
    (GRAPHICS : graphics^ ((require-library "graphicr.ss" "graphics")
			   (PLT : mzlib:file^)
			   (PLT : mred^)))
    (DRAW : coreDrawS (drawU ERR PLT GRAPHICS)))
   (export (open DRAW)
	   (open GRAPHICS))))

