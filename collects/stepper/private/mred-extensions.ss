(module mred-extensions mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           (prefix f: (lib "framework.ss" "framework"))
           (lib "pretty.ss")
           "highlight-placeholder.ss")

  (provide
   stepper-canvas%
   stepper-text%
   image?
   separator-snip% ;; these last two aren't required, but are useful 
   vertical-separator-snip%) ;; for debugging purposes

  (define test-dc (make-object bitmap-dc% (make-object bitmap% 1 1)))
  (define reduct-highlight-color (make-object color% 255 255 255))
  (define redex-highlight-color (make-object color% 255 255 255))
  (send test-dc try-color (make-object color% 212 159 245) reduct-highlight-color)
  (send test-dc try-color (make-object color% 193 251 181) redex-highlight-color)
  
  (define error-delta (make-object style-delta% 'change-style 'italic))
  (send error-delta set-delta-foreground "RED")
  
  (define snip-delta (make-object style-delta% 'change-alignment 'top))
  

;;;;;; copied from /plt/collects/drscheme/snip.ss :
  
  (define separator-snipclass
    (make-object
        (class snip-class% ()
          (override read)
          
          (define (read s)
            (let ([size-box (box 0)])
              (send s get size-box)
              (make-object separator-snip%)))
          
          (super-instantiate ()))))
  
  (send* separator-snipclass
    (set-version 1)
    (set-classname "drscheme:separator-snip%"))
  
  (send (get-the-snip-class-list) add separator-snipclass)
  
  ;; the two numbers 1 and 2 which appear here are to line up this snip
  ;; with the embedded snips around it in the drscheme rep.
  ;; I have no idea where the extra pixels are going.
  (define separator-snip%
    (class snip% ()
      (inherit get-style set-snipclass set-flags get-flags get-admin)
      (public reset-width)
      (override write copy get-extent draw)
      
      (define width 500)

      (define (reset-width)
        (let* ([admin (get-admin)]
               [reporting-media (send admin get-editor)]
               [reporting-admin (send reporting-media get-admin)]
               [widthb (box 0)])
          (send reporting-admin get-view #f #f widthb #f)
          (set! width (- (unbox widthb) 2))
          (send admin resized this #t)))
      
      (define (write s)
        (send s put (char->integer #\r)))
      
      (define (copy)
        (let ([s (make-object separator-snip%)])
          (send s set-style (get-style))
          s))
      
      (define height 1)
      (define white-around 2)
      
      (define (get-extent dc x y w-box h-box descent-box space-box lspace-box rspace-box)
        (for-each (lambda (box) (unless (not box) (set-box! box 0)))
                  (list descent-box space-box lspace-box rspace-box))
        (unless (not w-box)
          (set-box! w-box width))
        (unless (not h-box)
          (set-box! h-box (+ (* 2 white-around) height))))
      
      (define body-pen 
        (send the-pen-list find-or-create-pen
              "BLUE" 0 'solid))
      (define body-brush 
        (send the-brush-list find-or-create-brush
              "BLUE" 'solid))
      
      (define (draw dc x y left top right bottom dx dy draw-caret)
            (let ([orig-pen (send dc get-pen)]
                  [orig-brush (send dc get-brush)])
              (send dc set-pen body-pen)
              (send dc set-brush body-brush)
              
              (send dc draw-rectangle (+ x 1)
                    (+ white-around y) width height)
              
              (send dc set-pen orig-pen)
              (send dc set-brush orig-brush)))
      
      (super-instantiate ())
      (set-flags (cons 'hard-newline (get-flags)))
      (set-snipclass separator-snipclass)))

  ;;;; end of copied region

  ;;;; duplicated for vertical-snip
  
  (define red-arrow-bitmap
    (make-object bitmap% (build-path (collection-path "icons") "red-arrow.bmp") 'bmp))

  (unless (send red-arrow-bitmap ok?)
    (error 'red-arrow-bitmap "unable to load red-arrow bitmap"))
  
  (define vertical-separator-snipclass
    (make-object
     (class snip-class% ()
       (override read)
       
       (define (read s)
         (let ([size-box (box 0)])
           (send s get size-box)
           (make-object vertical-separator-snip% 100)))
       
       (super-instantiate ()))))
  
  (send* separator-snipclass
    (set-version 1)
    (set-classname "drscheme:vertical-separator-snip%"))
  
  (send (get-the-snip-class-list) add vertical-separator-snipclass)
  
  ;; the two numbers 1 and 2 which appear here are to line up this snip
  ;; with the embedded snips around it in the drscheme rep.
  ;; I have no idea where the extra pixels are going.
  (define vertical-separator-snip%
    (class snip% ()
      (inherit get-style set-snipclass set-flags get-flags get-admin)
      (public set-height!)
      (override write copy get-extent draw)
      
      (init height-init)
      (define height height-init)
      

      (define bitmap-width 15.0)
      (define left-white 0.0)
      (define right-white 3.0)
      (define bitmap-height 10.0)
      
      (define (set-height! x) 
        (set! height (max x bitmap-height)))
      
      (define (write s) 
        (send s put (char->integer #\r)))        ; this can't be right...?
      
      (define (copy)
        (let ([s (make-object vertical-separator-snip% height)])
          (send s set-style (get-style))
          s))
      
      (define (get-extent dc x y w-box h-box descent-box space-box lspace-box rspace-box)
        (for-each (lambda (box) (unless (not box) (set-box! box 0)))
                  (list descent-box space-box lspace-box rspace-box))
        (unless (not w-box)
          (set-box! w-box (+ left-white right-white bitmap-width)))
        (unless (not h-box)
          (set-box! h-box height)))
      
      (define (draw dc x y left top right bottom dx dy draw-caret)
        (let ([y-offset (round (/ (- height bitmap-height) 2))]
              [x-offset left-white])
          (send dc draw-bitmap red-arrow-bitmap (+ x x-offset) (+ y y-offset))))
      
      (super-instantiate ())
      (set-snipclass vertical-separator-snipclass)))
  
  
  ;;;; end of vertical snip-stuff
  
  (define stepper-editor-snip%
    (class editor-snip% ()
      (inherit get-editor set-min-width set-max-width)
      (public*
        [set-new-width
         (lambda (width canvas)
           (set-min-width width)
           (set-max-width width)
           (let ([editor (get-editor)])
             (when editor
               (send editor reset-pretty-print-width width canvas))))])
      (super-instantiate (#f #f 0 0 0 0 0 0 0 0))))
                                                                                                              
                                                                  ;                                    ;;    ;
       ;                                                          ;              ;                 ;  ;  ;  ; 
  ;;; ;;;;  ;;;   ; ;;;   ; ;;;    ;;;   ; ;;         ;;;  ;   ;  ; ;;;         ;;;;  ;;;  ;    ; ;;;;;  ; ;  
 ;     ;   ;   ;  ;;   ;  ;;   ;  ;   ;  ;;          ;     ;   ;  ;;   ;         ;   ;   ;  ;  ;   ;   ;; ;   
 ;     ;   ;   ;  ;    ;  ;    ;  ;   ;  ;           ;     ;   ;  ;    ;         ;   ;   ;   ;;    ;     ;    
  ;;   ;   ;;;;;  ;    ;  ;    ;  ;;;;;  ;    ;;;;;   ;;   ;   ;  ;    ;  ;;;;;  ;   ;;;;;   ;;    ;     ; ;; 
    ;  ;   ;      ;    ;  ;    ;  ;      ;              ;  ;   ;  ;    ;         ;   ;       ;;    ;    ; ;  ;
    ;  ;   ;      ;;   ;  ;;   ;  ;      ;              ;  ;  ;;  ;;   ;         ;   ;      ;  ;   ;   ;  ;  ;
 ;;;    ;;  ;;;;  ; ;;;   ; ;;;    ;;;;  ;           ;;;    ;; ;  ; ;;;           ;;  ;;;; ;    ;   ;;;    ;; 
                  ;       ;                                                                                   
                  ;       ;                                                                                   
                                                                                                              
  (define stepper-sub-text%
    (class f:text:basic% ()
      
      (init exps-init)
      (init highlights-init)
      (init highlight-color-init)
      
      (field [exps exps-init]
             [highlights highlights-init]
             [highlight-color highlight-color-init])

      (inherit insert get-style-list set-style-list change-style highlight-range last-position lock erase
               begin-edit-sequence end-edit-sequence get-start-position select-all clear)
      
      (public* [reset-pretty-print-width
                (lambda (inner-width canvas)
                  (begin-edit-sequence)
                  (let* ([style (send (get-style-list) find-named-style "Standard")]
                         [char-width (send style get-text-width (send canvas get-dc))]
                         [width (floor (/ (- inner-width 18) char-width))])
                    (reformat-sexp width)
                    (end-edit-sequence)))])
      
      (define pretty-printed-width #f)
      (define clear-highlight-thunks null)
      (define (reset-style)
        (change-style (send (get-style-list) find-named-style "Standard")))
               
      (define (reformat-sexp width)
        (when (not (eq? pretty-printed-width width))
          (set! pretty-printed-width width)
          (format-whole-step)))
               
      (define highlight-begin #f)
      (define remaining-highlights null)
      (define (highlight-pop)
        (if (null? remaining-highlights)
            (error 'highlight-pop "no highlighted region to match placeholder")
            (begin0 (car remaining-highlights) 
                    (set! remaining-highlights (cdr remaining-highlights))
                    (stack-top-decide))))
      (define next-is-placeholder? #f)
      (define (stack-top-decide)
        (set! next-is-placeholder?
              (or (null? remaining-highlights)
                  (confusable-value? (car remaining-highlights)))))
               
      (define (format-sexp sexp)
        (let ([real-print-hook (pretty-print-print-hook)])
          (parameterize ([pretty-print-columns pretty-printed-width]
                         [pretty-print-size-hook
                          (lambda (value display? port)
                            (if (eq? value highlight-placeholder) ; must be a confusable value
                                (string-length (format "~s" (car remaining-highlights)))
                                (if (image? value)
                                    1   ; if there was a good way to calculate a image widths ...
                                    #f)))]
                         [pretty-print-print-hook
                          (lambda (value display? port)
                            (if (eq? value highlight-placeholder)
                                (insert (format "~s" (car remaining-highlights)))
                                ; next occurs if value is an image:
                                (insert (send value copy))))]
                         [pretty-print-display-string-handler
                          (lambda (string port)
                            (insert string))]
                         [pretty-print-print-line
                          (lambda (number port old-length dest-columns)
                            (when (and number (not (eq? number 0)))
                              (insert #\newline))
                            0)]
                         [pretty-print-pre-print-hook
                          (lambda (value p)
                            (when (or (and (not next-is-placeholder?)
                                           (not (null? remaining-highlights))
                                           (eq? value (car remaining-highlights)))
                                      (eq? value highlight-placeholder))
                              (set! highlight-begin (get-start-position))))]
                         [pretty-print-post-print-hook
                          (lambda (value p)
                            (when (or (and (not next-is-placeholder?)
                                           (not (null? remaining-highlights))
                                           (eq? value (car remaining-highlights)))
                                      (eq? value highlight-placeholder))
                              (highlight-pop)
                              (let ([highlight-end (get-start-position)])
                                (unless highlight-begin
                                  (error 'format-whole-step "no highlight-begin to match highlight-end"))
                                (set! clear-highlight-thunks
                                      (cons (highlight-range highlight-begin highlight-end highlight-color #f #f)
                                            clear-highlight-thunks))
                                (set! highlight-begin #f))))])
            (pretty-print sexp))))
               
      (define (advance-substitute exp)
        (letrec ([stack-copy remaining-highlights]
                 [stack-pop (lambda () 
                              (if (null? stack-copy)
                                  (error 'advance-substitute "no highlighted region to fill placeholder")
                                  (begin0 (car stack-copy) (set! stack-copy (cdr stack-copy)))))]
                 [substitute
                  (lambda (exp)
                    (cond [(eq? exp highlight-placeholder)
                           (let ([popped (stack-pop)])
                             (if (confusable-value? popped)
                                 highlight-placeholder
                                 popped))]
                          [(pair? exp)
                           (cons (substitute (car exp)) (substitute (cdr exp)))]
                          [else
                           exp]))])
          (substitute exp)))
               
      (define (format-whole-step)
        (lock #f)
        (begin-edit-sequence)
        (for-each (lambda (fun) (fun)) clear-highlight-thunks)
        (set! clear-highlight-thunks null)
        (select-all)
        (clear)
        (reset-style)
        (set! remaining-highlights highlights)
        (stack-top-decide)
        (let loop ([remaining exps] [first #t])
          (unless (null? remaining)
            (unless first (insert #\newline))
            (format-sexp (advance-substitute (car remaining)))
            (loop (cdr remaining) #f)))
        (unless (null? remaining-highlights)
          (error 'format-whole-step "leftover highlights"))
        (end-edit-sequence)
        (lock #t))
      
      (super-instantiate ())
      (set-style-list (f:scheme:get-style-list))))
  
                                                   
                                                                  ;                                                                        ;;    ;
       ;                                                          ;                                                  ;                 ;  ;  ;  ; 
  ;;; ;;;;  ;;;   ; ;;;   ; ;;;    ;;;   ; ;;         ;;;  ;   ;  ; ;;;           ;;;   ; ;; ; ;;  ;;;   ; ;;       ;;;;  ;;;  ;    ; ;;;;;  ; ;  
 ;     ;   ;   ;  ;;   ;  ;;   ;  ;   ;  ;;          ;     ;   ;  ;;   ;         ;   ;  ;;   ;;   ;   ;  ;;          ;   ;   ;  ;  ;   ;   ;; ;   
 ;     ;   ;   ;  ;    ;  ;    ;  ;   ;  ;           ;     ;   ;  ;    ;         ;   ;  ;    ;    ;   ;  ;           ;   ;   ;   ;;    ;     ;    
  ;;   ;   ;;;;;  ;    ;  ;    ;  ;;;;;  ;    ;;;;;   ;;   ;   ;  ;    ;  ;;;;;  ;;;;;  ;    ;    ;   ;  ;    ;;;;;  ;   ;;;;;   ;;    ;     ; ;; 
    ;  ;   ;      ;    ;  ;    ;  ;      ;              ;  ;   ;  ;    ;         ;      ;    ;    ;   ;  ;           ;   ;       ;;    ;    ; ;  ;
    ;  ;   ;      ;;   ;  ;;   ;  ;      ;              ;  ;  ;;  ;;   ;         ;      ;    ;    ;   ;  ;           ;   ;      ;  ;   ;   ;  ;  ;
 ;;;    ;;  ;;;;  ; ;;;   ; ;;;    ;;;;  ;           ;;;    ;; ;  ; ;;;           ;;;;  ;    ;     ;;;   ;            ;;  ;;;; ;    ;   ;;;    ;; 
                  ;       ;                                                                                                                       
                  ;       ;                                                                                                                       
                                                                                                                                                  
  (define stepper-sub-error-text%
    (class f:text:basic% ()
      (init error-msg-init)
      
      (field [error-msg error-msg-init])
      
      (inherit get-style-list last-position set-style-list insert change-style auto-wrap
               set-max-width)
      
      (public* [reset-pretty-print-width 
                (lambda (inner-width canvas)
                  (set-max-width inner-width))])
      
      (super-instantiate ())
      (set-style-list (f:scheme:get-style-list))
      (let ([before-error-msg (last-position)])
        (change-style (send (get-style-list) find-named-style "Standard"))
        (auto-wrap #t)
        (insert error-msg)
        (change-style error-delta before-error-msg (last-position)))))
                                                                                                   
                                                                                            ;;    ;
       ;                                                                                   ;  ;  ; 
  ;;; ;;;;  ;;;   ; ;;;   ; ;;;    ;;;   ; ;;         ;;;   ;;;   ; ;;  ;   ;   ;;;    ;;; ;  ; ;  
 ;     ;   ;   ;  ;;   ;  ;;   ;  ;   ;  ;;          ;     ;   ;  ;;  ; ;   ;  ;   ;  ;     ;; ;   
 ;     ;   ;   ;  ;    ;  ;    ;  ;   ;  ;           ;         ;  ;   ;  ; ;       ;  ;       ;    
  ;;   ;   ;;;;;  ;    ;  ;    ;  ;;;;;  ;    ;;;;;  ;      ;;;;  ;   ;  ; ;    ;;;;   ;;     ; ;; 
    ;  ;   ;      ;    ;  ;    ;  ;      ;           ;     ;   ;  ;   ;  ; ;   ;   ;     ;   ; ;  ;
    ;  ;   ;      ;;   ;  ;;   ;  ;      ;           ;     ;   ;  ;   ;  ;;    ;   ;     ;  ;  ;  ;
 ;;;    ;;  ;;;;  ; ;;;   ; ;;;    ;;;;  ;            ;;;   ;;;;; ;   ;   ;     ;;;;; ;;;  ;    ;; 
                  ;       ;                                                                        
                  ;       ;                                                                        
                                                                                                   
  (define stepper-canvas%
    (class editor-canvas% ()
      (rename (super-on-size on-size))
      (inherit get-editor)
      (override*
        [on-size 
         (lambda (width height)
           (super-on-size width height)
           (let ([editor (get-editor)])
             (when editor
               (send editor reset-width this))))])
 
      (super-instantiate ())))
                                                                                  
                                                                           ;;    ;
       ;                                             ;                 ;  ;  ;  ; 
  ;;; ;;;;  ;;;   ; ;;;   ; ;;;    ;;;   ; ;;       ;;;;  ;;;  ;    ; ;;;;;  ; ;  
 ;     ;   ;   ;  ;;   ;  ;;   ;  ;   ;  ;;          ;   ;   ;  ;  ;   ;   ;; ;   
 ;     ;   ;   ;  ;    ;  ;    ;  ;   ;  ;           ;   ;   ;   ;;    ;     ;    
  ;;   ;   ;;;;;  ;    ;  ;    ;  ;;;;;  ;    ;;;;;  ;   ;;;;;   ;;    ;     ; ;; 
    ;  ;   ;      ;    ;  ;    ;  ;      ;           ;   ;       ;;    ;    ; ;  ;
    ;  ;   ;      ;;   ;  ;;   ;  ;      ;           ;   ;      ;  ;   ;   ;  ;  ;
 ;;;    ;;  ;;;;  ; ;;;   ; ;;;    ;;;;  ;            ;;  ;;;; ;    ;   ;;;    ;; 
                  ;       ;                                                       
                  ;       ;                                                       
                                                                                  
  ; constructor : ((listof sexp) (listof sexp) (listof sexp) (listof sexp) (listof sexp) (union string #f) (listof sexp) -> )
  
  ; redexes MUST NOT OVERLAP. all warranties void if this is violated.
  
  (define stepper-text%
    (class f:text:basic% ()
      (init finished-exprs-init exps-init redex-list-init post-exps-init reduct-list-init error-msg-init
            after-exprs-init)
      (field [finished-exprs finished-exprs-init]
             [exps exps-init]
             [redex-list redex-list-init]
             [post-exps post-exps-init]
             [reduct-list reduct-list-init]
             [error-msg error-msg-init]
             [after-exprs after-exprs-init])

      (inherit find-snip insert change-style highlight-range last-position lock erase auto-wrap
               begin-edit-sequence end-edit-sequence get-start-position get-style-list set-style-list
               get-admin get-snip-location get-dc needs-update hide-caret)
      (public* [reset-width 
                (lambda (canvas)
                  (begin-edit-sequence)
                  (let* ([width-box (box 0)]
                         [canvas-width (begin (send (get-admin) get-view #f #f width-box #f) (unbox width-box))]
                         [dc (send canvas get-dc)])
                    (unless (and old-width (= canvas-width old-width))
                      (set! old-width canvas-width)
                      (let* ([minus-cursor-margin (- canvas-width 2)]
                             [vert-separator-width-box (box 0)]
                             [_ (send vert-separator get-extent dc 0 0 vert-separator-width-box
                                      #f #f #f #f #f)]
                             [vert-separator-width (unbox vert-separator-width-box)]
                             [minus-center-bar (- minus-cursor-margin vert-separator-width)]
                             [l-r-box-widths (floor (/ minus-center-bar 2))])
                        (send top-defs-snip set-new-width minus-cursor-margin canvas)
                        (send before-snip set-new-width l-r-box-widths canvas)
                        (send after-snip set-new-width l-r-box-widths canvas)
                        (send bottom-defs-snip set-new-width minus-cursor-margin canvas)
                        (coordinate-snip-sizes)
                        (send horiz-separator-1 reset-width)
                        (send horiz-separator-2 reset-width)
                        (end-edit-sequence)))))])

      (define old-width #f)
      (define top-defs-snip (make-object stepper-editor-snip%))
      (define horiz-separator-1 (make-object separator-snip%))
      (define before-snip (make-object stepper-editor-snip%))
      (define vert-separator (make-object vertical-separator-snip% 10))
      (define after-snip (make-object stepper-editor-snip%))
      (define horiz-separator-2 (make-object separator-snip%))
      (define bottom-defs-snip (make-object stepper-editor-snip%))
      (define (release-snip-sizes)
        (for-each (lambda (snip)
                    (send snip set-min-height 0.0)
                    (send snip set-max-height 0.0)
                    (send snip set-max-height 'none))
                  (list before-snip after-snip)))
      (define (coordinate-snip-sizes)
        (let* ([get-snip-height
                (lambda (snip)
                  (let* ([top-box (box 0)]
                         [bottom-box (box 0)])
                    (get-snip-location snip #f top-box #f)
                    (get-snip-location snip #f bottom-box #t)
                    (- (unbox bottom-box) (unbox top-box))))]
               [max-height (apply max (map get-snip-height (list before-snip after-snip)))])
          (send vert-separator set-height! (- max-height 4))
          (let ([w-box (box 0)]
                [h-box (box 0)])
            (send vert-separator get-extent #f 0 0 w-box h-box #f #f #f #f)
            (needs-update vert-separator 0 0 (unbox w-box) (unbox h-box)))))
      
      

      (super-instantiate ())
      ;(hide-caret #t)
      (set-style-list (f:scheme:get-style-list))
      (let ([before-position (last-position)])
        (for-each (lambda (x) (insert x)) (list top-defs-snip (string #\newline) horiz-separator-1
                               before-snip vert-separator 
                               after-snip (string #\newline) 
                               horiz-separator-2 bottom-defs-snip))
        (change-style snip-delta before-position (last-position)))
      (send top-defs-snip set-editor 
            (make-object stepper-sub-text% finished-exprs null #f))
      (send before-snip set-editor
            (make-object stepper-sub-text% exps redex-list redex-highlight-color))
      (if (eq? error-msg #f)
          (send after-snip set-editor
                (make-object stepper-sub-text% post-exps reduct-list reduct-highlight-color))
          (send after-snip set-editor
                (make-object stepper-sub-error-text% error-msg)))
      (send bottom-defs-snip set-editor
            (make-object stepper-sub-text% after-exprs null #f))
      ;(lock #t)
      ))
  
  
  (define (image? val)
    (is-a? val snip%))
  
  (define (confusable-value? val)
    (or (number? val)
        (boolean? val)
        (string? val)
        (symbol? val))))

;(require mred-extensions)
;(require (lib "mred.ss" "mred"))
;
;(define (stepper-text-test . args)
;  (let* ([new-frame (make-object frame% "test-frame")]
;         [new-text (apply make-object stepper-text% args)]
;         [new-canvas (make-object stepper-canvas% new-frame new-text)])
;    (send new-canvas min-width 500)
;    (send new-canvas min-height 100)
;    (send new-frame show #t)
;    (send new-text reset-width new-canvas)
;    new-canvas))
;  
;  (define a
;  (stepper-text-test `((define x 3) 14)
;                     `((* 13 ,highlight-placeholder))
;                     `((* 15 16))
;                     `(,highlight-placeholder (define y 4) 13 (+ ,highlight-placeholder ,highlight-placeholder) 13
;                       298 (+ (x 398 ,highlight-placeholder) ,highlight-placeholder) ,highlight-placeholder)
;                     `((+ 3 4) 13 #f (+ x 398) (x 398 (+ x 398)) #f)
;                     #f
;                     `((define y (+ 13 14)) 80)))
  
;  (stepper-text-test `()
;                     `('uninteresting)
;                     `()
;                     `(13 ,highlight-placeholder)
;                     `(13)
;                     #f
;                     `())
  
  
; this should be an error:
;(stepper-text-test `()
;                   `('uninteresting)
;                   `(too-many-fill-ins) `() `() #f `())

; so should this:
;(stepper-text-test `()
;                   `(uninteresting)
;                   `() `(,highlight-placeholder ,highlight-placeholder) `(too-few-fill-ins) #f `())
  
;  (stepper-text-test `() `(uninteresting but long series of lines) `() `() `() "This is an error message" `((define x 3 4 5)))
  
;(stepper-text-test `() `() `() `() `() "This is another error message" `(poomp))
