(define-signature graphics^
  (viewport?

   open-graphics 
   close-graphics 
   graphics-open?

   make-posn posn? posn-x posn-y

   get-mouse-click
   ready-mouse-click
   ready-mouse-release
   mouse-click-posn
   query-mouse-posn
   viewport-flush-input
   left-mouse-click?
   middle-mouse-click?
   right-mouse-click?

   get-key-press
   ready-key-press
   key-value

   make-rgb
   rgb-blue rgb-red rgb-green
   change-color 
   rgb?

   open-viewport 
   open-pixmap
   close-viewport    

   clear-viewport draw-viewport flip-viewport

   set-viewport-scale

   draw-line clear-line flip-line 
   draw-pixel clear-pixel flip-pixel get-pixel 

   draw-rectangle clear-rectangle flip-rectangle 
   draw-ellipse clear-ellipse flip-ellipse 
   draw-polygon clear-polygon flip-polygon 
   draw-solid-rectangle clear-solid-rectangle flip-solid-rectangle 
   draw-solid-ellipse clear-solid-ellipse flip-solid-ellipse 
   draw-solid-polygon clear-solid-polygon flip-solid-polygon 

   get-string-size

   draw-string clear-string flip-string 

   draw-pixmap-posn
   draw-pixmap

   copy-viewport 

   default-display-is-color?

   viewport->snip

   viewport-DC viewport-buffer-DC))
