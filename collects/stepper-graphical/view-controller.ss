(unit/sig (stepper-go)
  (import [c : mzlib:core^]
          [e : zodiac:interface^]
          [z : zodiac:system^]
          [cp : stepper:client-procs^]
          mred^
          [d : drscheme:export^]
          [p : mzlib:print-convert^]
          [f : framework^]
          stepper:shared^
          [utils : stepper:cogen-utils^]
          [marks : stepper:marks^]
          [x : stepper:mred-extensions^])

  (define stepper-initial-width 500)
  (define stepper-initial-height 500)

  (define image? x:image?)
  
  (define stepper-frame%
    (class (d:frame:basics-mixin (f:frame:standard-menus-mixin f:frame:basic%)) (drscheme-frame)
      (rename [super-on-close on-close])
      (public
        [set-printing-proc 
         (lambda (proc)
           (set! printing-proc proc))]
        [printing-proc (lambda (item evt)
                         (printf "shouldn't be called~n"))])
      (override
        [file-menu:print (lambda (a b) (printing-proc a b))] 
        [on-close
         (lambda ()
           (send drscheme-frame stepper-frame #f)
           (super-on-close))])
      (sequence (super-init "Stepper" #f stepper-initial-width stepper-initial-height))))
  
  (define (stepper-wrapper drscheme-frame settings)
    
    (local ((define view-history null)
            (define view-currently-updating #f)
            (define final-view #f)
            (define view 0)
            
            ; build gui object:
            
            (define (home)
              (update-view 0))
            
            (define (next)
              (send next-button enable #f)
              (send previous-button enable #f)
              (send home-button enable #f)
              (if (= view (- (length view-history) 1))
                  (update-view/next-step (+ view 1))
                  (update-view (+ view 1))))
            
            (define (previous)
              (update-view (- view 1)))
            
            (define s-frame (make-object stepper-frame% drscheme-frame))
            
            (define button-panel (make-object horizontal-panel% (send s-frame get-area-container)))
            (define home-button (make-object button% "Home" button-panel
                                             (lambda (_1 _2) (home))))
            (define previous-button (make-object button% "<< Previous" button-panel
                                                 (lambda (_1 _2) (previous))))
            (define next-button (make-object button% "Next >>" button-panel (lambda
                                                                                (_1 _2) (next))))
            
            (define canvas (make-object x:stepper-canvas% (send s-frame get-area-container)))
            
            (define (update-view/next-step new-view)
              (set! view-currently-updating new-view)
              (step))
            
            (define (update-view new-view)
              (set! view new-view)
              (let ([e (list-ref view-history view)])
                (send canvas set-editor e)
                (send e reset-width canvas)
                (send e set-position (send e last-position)))
              (send previous-button enable (not (zero? view)))
              (send home-button enable (not (zero? view)))
              (send next-button enable (not (eq? final-view view))))
            
            (define (print-current-view item evt)
              (send (send canvas get-editor) print))
            
            (define (receive-result result)
              (let ([step-text
                     (cond [(before-after-result? result) 
                            (make-object x:stepper-text% 
                                         (before-after-result-finished-exprs result)
                                         (before-after-result-exp result)
                                         (before-after-result-redex result)
                                         (before-after-result-post-exp result)
                                         (before-after-result-reduct result)
                                         #f
                                         (before-after-result-after-exprs result))]
                           [(before-error-result? result)
                            (set! final-view view-currently-updating)
                            (make-object x:stepper-text%
                                         (before-error-result-finished-exprs result)
                                         (before-error-result-exp result)
                                         (before-error-result-redex result)
                                         null
                                         null
                                         (before-error-result-err-msg result)
                                         (before-error-result-after-exprs result))]
                           [(error-result? result)  
                            (set! final-view view-currently-updating)
                            (make-object x:stepper-text%
                                         (error-result-finished-exprs result)
                                         null
                                         null
                                         null
                                         null
                                         (error-result-err-msg result)
                                         null)]
                           [(finished-result? result)
                            (set! final-view view-currently-updating)
                            (make-object x:stepper-text%
                                         (finished-result-finished-exprs result)
                                         null
                                         null
                                         null
                                         null
                                         #f
                                         null)])])
                (set! view-history (append view-history (list step-text))) 
                (update-view view-currently-updating)))
            
            (define text-stream
              (f:gui-utils:read-snips/chars-from-text (ivar drscheme-frame definitions-text)))
            
            (define step 
              (invoke-unit/sig (require-library-unit/sig "instance.ss" "stepper-graphical")
                               stepper:model-input^
                               (c : mzlib:core^)
                               (e : zodiac:interface^)
                               (p : mzlib:print-convert^)
                               (d : drscheme:export^)
                               (z : zodiac:system^)
                               (cp : stepper:client-procs^)
                               stepper:shared^
                               mred^
                               (utils : stepper:cogen-utils^)
                               (marks : stepper:marks^))))
      
      (send drscheme-frame stepper-frame s-frame)
      (send s-frame set-printing-proc print-current-view)
      (set! view-currently-updating 0)
      (send button-panel stretchable-width #f)
      (send button-panel stretchable-height #f)
      (send canvas stretchable-height #t)
      (send previous-button enable #f)
      (send home-button enable #f)
      (send next-button enable #f)
      (send (send s-frame edit-menu:get-undo-item) enable #f)
      (send (send s-frame edit-menu:get-redo-item) enable #f)
      (step)
      (send s-frame show #t)))
  
  (define beginner-level-name "Beginning Student")
  (define intermediate-level-name "Intermediate Student")
      
  (define (stepper-go frame)
    (let ([settings (f:preferences:get d:language:settings-preferences-symbol)])
      (if (not (or (string=? (d:basis:setting-name settings) beginner-level-name)
                   (string=? (d:basis:setting-name settings) intermediate-level-name)))
          (message-box "Stepper" 
                       (format (string-append "Language level is set to \"~a\".~n"
                                              "The stepper only works for the \"~a\" and the~n"
                                              "\"~a\" language levels.~n")
                               (d:basis:setting-name settings)
                               beginner-level-name
                               intermediate-level-name)
                       #f 
                       '(ok))
          (stepper-wrapper frame settings)))))
