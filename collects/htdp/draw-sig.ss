;; xxx-solid-rect cannot be called xxx-solid-rectangle because that
;; interferes with the existing xxx-solid-rectangle name in our unit
;; calculus -- mf 

(module draw-sig mzscheme
  (provide coreDrawS drawS)
  (require (lib "unitsig.ss"))
  
  (define-signature coreDrawS
    (start stop 
           draw-circle draw-solid-disk draw-solid-rect draw-solid-line
           clear-circle clear-solid-disk clear-solid-rect clear-solid-line
           clear-all
           sleep-for-a-while
           wait-for-mouse-click ; -> posn
           ;WHITE YELLOW RED BLUE GREEN BLACK
           get-@VP))
  
  (define-signature drawS coreDrawS))
