(require-library "error.ss" "htdp")
(require-library "big-draw.ss" "htdp")

(define-signature graphS (graph-fun graph-line))

(define graphU
  (unit/sig graphS
    (import errorS bigDrawS plt:userspace^)

    ;; make-graph : sym -> void
    ;; effect: set up pasteboard for drawing functions 
    ;;   between [0,10] and [0,10] on x/y axis 
    (define (make-graph name)
      (start EAST SOUTH)
      (draw-solid-line ORIGIN X-AXIS 'blue)
      ((draw-string @VP) (make-posn (+ OFFSET 10) (+ OFFSET 10)) "Y-AXIS")
      (draw-solid-line ORIGIN Y-AXIS 'blue)
      ((draw-string @VP) (make-posn (- EAST 100) (- SOUTH 15)) "X-AXIS"))

    ;; graph-line : (num -> num) symbol -> true
    ;; effect: draw function graph for x in [0,10] at delta = .1
    (define (graph-line f color)
      (check 'graph-line f color)
      (let ((p1 (translate (make-posn 0 (f 0))))
	    (p2 (translate (make-posn 10 (f 10)))))
	(draw-solid-line p1 p2 color)))

    ;; graph-fun : (num -> num) symbol -> true
    ;; effect: draw function graph for x in [0,10] at delta = .1
    (define (graph-fun f color)
      (check 'graph-fun f color)
      (draw-tab (map translate (tabulate f 0 10 DELTA)) color))

    ;; check : tst tst tst -> void
    (define (check tag f color)
      (check-proc tag f 1 '1st "one argument")
      (check-arg tag (rgb? color) 'color '2nd color))

    ;; tabulate : (num -> num) num num num -> (list-of (make-posn num num))
    (define (tabulate f left right delta)
      (if (> left right) null
	(cons (make-posn left (f left))
	  (tabulate f (+ left delta) right delta))))

    ;; translate : posn -> posn
    (define (translate p)
      (make-posn (+ (* FACT (/ 1 DELTA) (posn-x p)) OFFSET) 
	(- (- SOUTH (* FACT (/ 1 DELTA) (posn-y p))) OFFSET)))

    ;; draw-tab : (list-of (make-posn num num)) symbol -> true
    (define (draw-tab lop color)
      (for-each (lambda (p) (draw-solid-disk p DOT color)) lop)
      #t)

    (define EAST  400)
    (define SOUTH EAST)
    (define FACT  (/ (- EAST 100) 100))
    (define OFFSET   10.)
    (define ORIGIN (make-posn OFFSET          (- SOUTH OFFSET)))
    (define X-AXIS (make-posn OFFSET OFFSET))
    (define Y-AXIS (make-posn (- EAST OFFSET) (- SOUTH OFFSET)))
    (define GRAPH-COLOR 'red)
    
    (define DELTA .1)
    (define DOT   1)
    
    (make-graph 'ok)))

(compound-unit/sig (import (PLT : plt:userspace^))
  (link
    [ERR   : errorS (errorU)]   
    [DRAW  : bigDrawS  (bigDrawU ERR PLT)]
    [PINGP : graphS (graphU ERR DRAW PLT)])
  (export
    (open DRAW)
    (open PINGP)))
