#cs(module big-draw mzscheme
     (require "error.ss"
              "draw-sig.ss"
              (lib "etc.ss")
              (lib "posn.ss" "lang")
              (lib "prim.ss" "lang")           
              (lib "unitsig.ss")
              (prefix mred: (lib "mred.ss" "mred"))
              (lib "mred-sig.ss" "mred")
              (lib "graphics-sig.ss" "graphics")
              (lib "graphics-posn-less-unit.ss" "graphics"))
     
     (define-values/invoke-unit/sig graphics:posn-less^
                                    graphics-posn-less@ #f 
                                    (mred : mred^)
                                    graphics:posn^)
     
     (provide-signature-elements graphics:posn-less^)
     
     (define-primitive draw-solid-disk draw-solid-disk/proc)
     (define-primitive draw-circle draw-circle/proc)
     (define-primitive draw-solid-rect draw-solid-rect/proc)
     (define-primitive draw-solid-line draw-solid-line/proc)  
     
     (define-primitive clear-solid-disk clear-solid-disk/proc)
     (define-primitive clear-circle clear-circle/proc)
     (define-primitive clear-solid-rect clear-solid-rect/proc)
     (define-primitive clear-solid-line clear-solid-line/proc)  
     (define-primitive clear-all clear-all/proc)
     
  ;   (provide draw-solid-string clear-solid-string)

     (define-primitive draw-solid-string draw-string/proc)
     (define-primitive clear-solid-string clear-string/proc)

     (define-primitive sleep-for-a-while sleep-for-a-while/proc)
     (define-primitive wait-for-mouse-click wait-for-mouse-click/proc)
     (define-primitive get-key-event get-key-event/proc)     
     
     (define the-error
       (lambda x
         (error "evaluate (start <num> <num>) first")))  
     
     (define %draw-solid-disk the-error)
     (define draw-solid-disk/proc (lambda a (apply %draw-solid-disk a)))
     
     (define %clear-solid-disk the-error)
     (define clear-solid-disk/proc (lambda a (apply %clear-solid-disk a)))
     
     (define %draw-circle the-error)
     (define draw-circle/proc (lambda a (apply %draw-circle a)))
     
     (define %clear-circle the-error)
     (define clear-circle/proc (lambda a (apply %clear-circle a)))
     
     (define %draw-string the-error)
     (define draw-string/proc (lambda a (apply %draw-string a)))
     
     (define %clear-string the-error)
     (define clear-string/proc (lambda a (apply %clear-string a)))
     
     (define %draw-solid-rect the-error)
     (define draw-solid-rect/proc (lambda a (apply %draw-solid-rect a)))
     
     (define %clear-solid-rect the-error)
     (define clear-solid-rect/proc (lambda a (apply %clear-solid-rect a)))
     
     (define %draw-solid-line the-error) 
     (define draw-solid-line/proc (lambda a (apply %draw-solid-line a)))
     
     (define %clear-solid-line the-error)
     (define clear-solid-line/proc (lambda a (apply %clear-solid-line a)))
     
     (define %clear-all the-error)
     (define clear-all/proc (lambda a (apply %clear-all a)))
     
     (define %wait-for-mouse-click the-error)
     (define (wait-for-mouse-click/proc) (%wait-for-mouse-click))
     
     (define %get-key-event the-error)
     (define (get-key-event/proc) (%get-key-event))
     
     (define (make-true f) (lambda x (apply f x) #t))
     
     (define sleep-for-a-while/proc (make-true mred:sleep/yield))
     
     ;; i wish i could abstract these functions ...
     (define (make-line name f)
       (make-true
        (lambda x
          (apply (lambda (p1 p2 . c)
                   (check-arg name (posn? p1) "posn" "first" p1)
                   (check-arg name (posn? p2) "posn" "second" p2)
                   (f p1 p2 (check-optional name 3 c "third" x)))
                 x))))
     
     (define (make-rect name f)
       (make-true
        (lambda x
          (apply (lambda (p w h . c)
                   (check-arg name (posn? p) "posn" "first" p)
                   (check-arg
                    name (and (integer? w) (> w 0)) "positive integer" "second" w)
                   (check-arg
                    name (and (integer? h) (> h 0)) "positive integer" "third" h)
                   (f p w h (check-optional name 4 c "fourth" x)))
                 x))))
     
     ;; Local function for make-circle 
     ;; (Posn Number Symbol[color] -> void) -> (Posn Number Symbol[color] -> void)
     (define (ellipsis-2-circle f)
       (lambda (p r c)
         (let ((d (* r 2)))
           (f (make-posn (- (posn-x p) r) (- (posn-y p) r)) d d c))))
     
     (define (make-circle name f)
       (make-true
        (lambda x
          (apply (lambda (p r . c)
                   (check-arg name (posn? p) "posn" "first" p)
                   (check-arg 
                    name (and (integer? r) (> r 0)) "positive integer" "second" r)
                   ((ellipsis-2-circle f) p r (check-optional name 3 c "third" x)))
                 x))))

     (define (make-%string name f)
       (make-true
        (lambda x
          (apply (lambda (p s)
                   (check-arg name (posn? p) "posn" "first" p)
                   (check-arg name (string? s) "string" "second" s)
                   (f p s))
                 x))))
     
     ;; (Listof _) String (Listof _) -> Symbol[color]
     ;; contract: c is shared suffix of all
     ;; check whether c contains a single color symbol and all has proper length
     (define (check-optional name n c position x)
       (if (pair? c)
           (begin
             (check-arity name n x)
             (check-arg name (symbol? (car c)) "symbol" position (car c)))
           (check-arity name (- n 1) x))
       (symbol->color (if (null? c) 'black (car c))))
     
     (define (start WIDTH HEIGHT)
       (check-arg 'start (and (integer? WIDTH) (> WIDTH 0)) "positive integer" "first" WIDTH)
       (check-arg 'start (and (integer? HEIGHT) (> HEIGHT 0)) "positive integer" "second" HEIGHT)
       ;; --- 
       (open-graphics)
       (let ((current-window (open-viewport "Canvas" WIDTH HEIGHT)))
         (set! @vp current-window)
         (set! %clear-all (clear-viewport current-window))
         
         (set! %draw-solid-line
               (make-line 'draw-solid-line
                          (draw-line current-window)))
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
                              ((clear-ellipse current-window) p r1 r2))))


	 (set! %draw-string (make-%string 'draw-string (draw-string current-window)))
         (set! %clear-string (make-%string 'clear-string (clear-string current-window)))

         
         (set! %wait-for-mouse-click
               (lambda ()
                 (mouse-click-posn
                  (get-mouse-click @vp))))
         
         (set! %get-key-event
               (lambda ()
                 (cond
                   [(ready-key-press @vp) => key-value]
                   [else false])))

         #t))
     
     (define (stop)
       (close-graphics)
       (set! @vp #f)
       (set! %clear-all the-error)
       
       (set! %draw-solid-line the-error)
       (set! %clear-solid-line the-error)
       
       (set! %draw-solid-rect the-error)
       (set! %clear-solid-rect the-error)
       
       (set! %draw-solid-disk the-error)
       (set! %clear-solid-disk the-error)
       
       (set! %draw-circle the-error)
       (set! %clear-circle the-error)
       
       (set! %wait-for-mouse-click the-error)
       
       (set! %get-key-event the-error)
       #t)
     
     
     
     ;; start/cartesian-plane : Number Number -> true
     ;; start up a canvas of size width x height  and draw a centered cartesian coordinate
     (define (start/cartesian-plane width height)
       (check-arg 'start/cartesian-plane
                  (and (integer? width) (> width 0)) "positive integer" "first" width)
       (check-arg 'start/cartesian-plane
                  (and (integer? height) (> height 0)) "positive integer" "second" height)    
       (local ((define trash  (start width height))
               (define mid-x  (quotient width 2))
               (define mid-y  (quotient height 2)))
         (and (draw-solid-line (make-posn mid-x 0) (make-posn mid-x height))
              (draw-solid-line (make-posn 0 mid-y) (make-posn width mid-y)))))
     
     (define @vp #f)
     #cs(define (get-@VP) @vp)
     
     (provide-signature-elements draw^)
     
     ;; symbol->color : symbol -> color
     ;; to convert symbol to 
     (define (symbol->color s)
       (check-arg 'draw.ss (symbol? s) "symbol" "first" s)
       (case s 
         ((white)   (make-rgb 1 1 1))
         ((yellow)  (make-rgb 1 1 0))
         ((red)     (make-rgb 1.0 0 0))
         ((green)   (make-rgb 0 1.0 0))
         ((blue)    (make-rgb 0 0 1.0))
         ((black)   (make-rgb 0 0 0))
         (else (error 'draw.ss "The symbol ~e is not a legal color in draw.ss." s)))))
   
   
