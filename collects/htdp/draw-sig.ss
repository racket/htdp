;; xxx-solid-rect cannot be called xxx-solid-rectangle because that
;; interferes with the existing xxx-solid-rectangle name in our unit
;; calculus -- mf 

(define-signature coreDrawS
  (start stop 
   draw-circle draw-solid-disk draw-solid-rect draw-solid-line
   clear-circle clear-solid-disk clear-solid-rect clear-solid-line
   clear-all
   sleep-for-a-while
   WHITE YELLOW RED BLUE GREEN BLACK
   @VP))

(define-signature drawS
  ((open coreDrawS)
   make-posn posn? posn-x posn-y))
