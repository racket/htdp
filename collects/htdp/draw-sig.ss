#cs(module draw-sig mzscheme
  (provide core-draw^ draw^)
  (require (lib "unitsig.ss"))
  
;; xxx-solid-rect cannot be called xxx-solid-rectangle because that
;; interferes with the existing xxx-solid-rectangle name in our unit
;; calculus -- mf 


  (define-signature core-draw^
       (start 
        start/cartesian-plane
        get-key-event
        stop 
        draw-circle draw-solid-disk draw-solid-rect draw-solid-line
	draw-solid-string
        clear-circle clear-solid-disk clear-solid-rect clear-solid-line
	clear-solid-string
        clear-all
        sleep-for-a-while
        wait-for-mouse-click ; -> posn
        get-@VP
        ))
  
  (define-signature draw^ core-draw^))
