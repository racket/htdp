;; won't work -- imperative.


(require-library "math.ss")
(require-library "function.ss")
(define-struct turtle (x y angle))
  ; x : int
  ; y: int
  ; angle : int

  (define-struct cached (turtles cache))
  ; turtles : (list-of turtle)
  ; cache : turtle -> turtle

  (define-struct tree (children))
  ; children : (list-of cached)

  
  (define pi/2 (/ pi 2))
(define (set-box/f b v) (when (box? b) (set-box! b v)))
  
(define icon-pen (send the-pen-list find-or-create-pen "SALMON" 1 'xor))
(define icon-brush (send the-brush-list find-or-create-brush "SALMON" 'xor))
(define blank-pen (send the-pen-list find-or-create-pen "BLACK" 1 'transparent))
(define w-pen (send the-pen-list find-or-create-pen "white" 1 'solid))
(define b-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
(define w-brush (send the-brush-list find-or-create-brush "WHITE" 'solid))

;; cache elements:
(define-struct c-forward (distance))
(define-struct c-turn (angle))
(define-struct c-draw (distance))
(define-struct c-offset (x y))

(define empty-cache (make-turtle 0 0 0))

(define turtle-snip%
  (class snip% (width height)
    (private
      [turtle-style 'triangle]
      [bitmap (make-object bitmap% width height #t)]
      [memory-dc (make-object bitmap-dc% bitmap)]
      [turtles-state (list (make-turtle 
                            (quotient width 2)
                            (quotient height 2)
                            0))]
      [turtles-cache empty-cache])
    (public
      [set-turtles-state
       (lambda (ts)
         (set! turtles-state tc))]
      [set-turtles-cache
       (lambda (tc)
         (set! turtles-cache tc))]
      [set-bitmap
       (lambda (b)
         (let ([bw (send b get-width)]
               [bh (send b get-height)])
           (unless (and (= bw width)
                        (= bh height))
             (set! bitmap (make-object bitmap% bw bh))
             (send memory-dc set-bitmap bitmap))
           (send memory-dc draw-bitmap bitmap 0 0)))])
    (private
      [pl (make-object point% 0 0)]
      [pr (make-object point% 0 0)]
      [ph (make-object point% 0 0)]
      [points (list pl pr ph)]
      [flip-icons
       (lambda (dc dx dy)
         (case turtle-style
           [(triangle line)
            (let* ([proc
                    (if (eq? turtle-style 'line)
                        (lambda (turtle)
                          (let ([x (turtle-x turtle)]
                                [y (turtle-y turtle)]
                                [theta (turtle-angle turtle)]
                                [size 2])
                            (send dc draw-line
                                  (+ dx x)
                                  (+ dy y)
                                  (+ dx x (* size (cos theta)))
                                  (+ dy y (* size (sin theta))))))
                        (lambda (turtle)
                          (let* ([x (turtle-x turtle)]
                                 [y (turtle-y turtle)]
                                 [theta (turtle-angle turtle)]
                                 [long-size 20]
                                 [short-size 7]
                                 [l-theta (+ theta pi/2)]
                                 [r-theta (- theta pi/2)])
                            (send ph set-x (+ dx x (* long-size (cos theta))))
                            (send ph set-y (+ dy y (* long-size (sin theta))))
                            (send pl set-x (+ dx x (* short-size (cos l-theta))))
                            (send pl set-y (+ dy y (* short-size (sin l-theta))))
                            (send pr set-x (+ dx x (* short-size (cos r-theta))))
                            (send pr set-y (+ dy y (* short-size (sin r-theta))))
                            (send dc draw-polygon points))))])
              (if (eq? turtle-style 'line)
                  (send dc set-pen icon-pen)
                  (begin
                    (send dc set-pen blank-pen)
                    (send dc set-brush icon-brush)))
              (for-each proc turtles-state)
              (send dc set-pen b-pen))]
           [else
            (void)]))]
      [clear
       (lambda () 
         (send memory-dc clear))])
    
    (public
      [save-turtle-bitmap
       (lambda (fn type)
         (send bitmap save-file fn type))])
    
    (public
      [wipe-line (let* ([dc-line (ivar memory-dc draw-line)]
                        [dc-pen (ivar memory-dc set-pen)])
                   (lambda (a b c d)
                     (dc-pen w-pen)
                     (dc-line a b c d)
                     (dc-pen b-pen)))]
      [draw-line (let* ([dc-line (ivar memory-dc draw-line)]
                        [dc-pen (ivar memory-dc set-pen)])
                   (lambda (a b c d)
                     (dc-line a b c d)))])
    (override
      [copy
       (lambda ()
         (let ([t (make-object turtle-snip% width height)])
           (flatten (lambda (x) x))
           (send t set-turtles-state turtles-state)
           (send t set-bitmap bitmap)
           t))]
      [draw
       (lambda (dc x y left top right bottom dx dy draw-caret)
         (flatten (lambda (x) x))
         (let ([old-pen (send dc get-pen)]
               [old-brush (send dc get-brush)]
               [old-clip (send dc get-clipping-region)])
           (send dc set-pen b-pen)
           (send dc set-brush w-brush)
           (send dc draw-rectangle x y (+ width 2) (+ height 2))
           (send dc set-clipping-rect (+ x 1) (+ y 1) height width)
           (send dc draw-bitmap (send memory-dc get-bitmap) (+ x 1) (+ y 1))
           (flip-icons dc (+ x 1) (+ y 1))
           (send dc set-pen old-pen)
           (send dc set-brush old-brush)
           (send dc set-clipping-region old-clip)))]
      [get-extent
       (lambda (dc x y w h descent space lspace rspace)
         (set-box/f w (+ width 2))
         (set-box/f h (+ height 2))
         (set-box/f descent 0)
         (set-box/f space 0)
         (set-box/f lspace 0)
         (set-box/f rspace 0))])
    
    (public
      [flatten
       (lambda (at-end)
         (letrec ([walk-turtles
                   (lambda (turtles cache list)
                     (cond
                       [(tree? turtles)
                        (let ([children (tree-children turtles)]
                              [ac (apply-cache cache)])
                          (foldl (lambda (child list)
                                   (walk-turtles (cached-turtles child)
                                                 (ac (cached-cache child))
                                                 list))
                                 list
                                 children))]
                       [else
                        (let ([f (compose at-end (apply-cache cache))])
                          (foldl (lambda (t l) (cons (f t) l)) list turtles))]))])
           (set! turtles-state (walk-turtles turtles-state turtles-cache null))
           (set! turtles-cache empty-cache)))])
    
    (sequence
      (super-init)
      (clear))))

  ;; combines a cache-element and a turtle-offset.
  ;; turtle-offsets are represented as turtles, 
  ;; however they are deltas, not absolutes.
  (define combine
    (lambda (entry cache)
      (cond 
	[(c-forward? entry)
	 (let* ([n (c-forward-distance entry)]
		[angle (turtle-angle cache)]
		[x (turtle-x cache)]
		[y (turtle-y cache)]
		[newx (+ x (* n (cos angle)))]
		[newy (+ y (* n (sin angle)))])
	   (make-turtle newx newy angle))]
	[(c-offset? entry)
	 (let* ([tx (turtle-x cache)]
		[ty (turtle-y cache)]
		[newx (+ tx (c-offset-x entry))]
		[newy (+ ty (c-offset-y entry))])
	   (make-turtle newx newy 
			(turtle-angle cache)))]
	[(c-turn? entry)
	 (make-turtle (turtle-x cache)
		      (turtle-y cache)
		      (- (turtle-angle cache)
			 (c-turn-angle entry)))]
	[else
	 (error 'turtles-cache "illegal entry in cache: ~a" entry)])))
  
  ;; this applies an offset to a turtle.
  ;; an offset is a turtle, representing what would happen 
  ;;    if the turtle had started at zero.
  (define apply-cache
    (lambda (offset)
      (let ([x (turtle-x offset)]
	    [y (turtle-y offset)]
	    [offset-angle (turtle-angle offset)])
	(lambda (turtle)
	  (let* ([angle (turtle-angle turtle)])
	    (let* ([c (cos angle)]
		   [s (sin angle)]
		   [rx (- (* x c) (* y s))]
		   [ry (+ (* y c) (* x s))])
	      (make-turtle (+ rx (turtle-x turtle))
			   (+ ry (turtle-y turtle))
			   (+ offset-angle angle))))))))
  
  (define draw/erase
    (lambda (doit)
      (lambda (n tv)
	(send tv flatten
              (lambda (turtle)
                (let* ([x (turtle-x turtle)]
                       [y (turtle-y turtle)]
                       [angle (turtle-angle turtle)]
                       [d (if (zero? n) 0 (sub1 (abs n)))]
                       [res (if (< n 0) (- d) d)]
                       [c (cos angle)]
                       [s (sin angle)]
                       [drawx (+ x (* res c))]
                       [drawy (+ y (* res s))]
                       [newx (+ x (* n c))]
                       [newy (+ y (* n s))])
                  (unless (zero? n)
                    (doit tv x y drawx drawy))
                  (make-turtle newx newy angle))))
        tv)))

  (define draw (draw/erase (lambda (tv a b c d) (send tv draw-line a b c d))))
  (define erase (draw/erase (lambda (tv a b c d) (send tv wipe-line a b c d))))
  
  (define move
    (lambda (n tv)
      (send tv set-turtles-cache (combine (make-c-forward n) turtles-cache))))
  
  (define turn/radians
    (lambda (d tv)
      (send tv set-turtles-cache (combine (make-c-turn d) turtles-cache))))
  
  (define turn
    (lambda (c)
      (turn/radians (* (/ c 360) 2 pi))))
  
  (define move-offset
    (lambda (x y tv)
      (send tv set-turtles-cache (combine (make-c-offset x y) turtles-cache))))
  
  (define erase/draw-offset
    (lambda (doit)
      (lambda (x y tv)
	(send tv flatten
              (lambda (turtle)
                (let* ([tx (turtle-x turtle)]
                       [ty (turtle-y turtle)]
                       [newx (+ tx x)]
                       [newy (+ ty y)])
                  (doit tx ty newx newy)
                  (make-turtle newx newy (turtle-angle turtle))))))))
  
  (define erase-offset (erase/draw-offset (lambda (a b c d) (wipe-line a b c d))))
  (define draw-offset (erase/draw-offset (lambda (a b c d) (line a b c d))))
  

(let* ([f (make-object frame% "frame" #f 400 400)]
       [t (make-object text%)]
       [c (make-object editor-canvas% f t)])
  (send t insert (draw 10 (make-object turtle-snip% 30 30)))
  (send t insert (make-object turtle-snip% 150 150))
  
  (send f show #t))
