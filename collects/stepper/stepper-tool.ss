(module stepper-tool mzscheme
  (require (lib "specs.ss" "framework")
           (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred")  
           (prefix frame: (lib "framework.ss" "framework"))
           (lib "unitsig.ss")
           (lib "class.ss")
           (lib "etc.ss")
           (lib "list.ss")
           (prefix model: "private/model.ss")
           (prefix x: "private/mred-extensions.ss")
           "private/shared.ss")
  
  (provide tool@)
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)

      (define (phase1) (void))
      (define (phase2) (void))
      
      (define stepper-initial-width 500)
      (define stepper-initial-height 500)
      
      (define image? x:image?)
  
      (define stepper-frame%
        (class (drscheme:frame:basics-mixin (frame:frame:standard-menus-mixin frame:frame:basic%))
          
          (init-field drscheme-frame)
          (rename [super-on-close on-close])
          (public set-printing-proc)
          (override  on-close) ; file-menu:print
          
          (define (set-printing-proc proc)
            (set! printing-proc proc))
          
          (define (printing-proc item evt)
            (message-box "error?" "shouldn't be called"))
          
          (define (file-menu:print a b) (printing-proc a b))
          
          (define (on-close)
            (send drscheme-frame stepper-frame #f)
            (super-on-close))
          
          (super-instantiate ("Stepper" #f stepper-initial-width stepper-initial-height))))
  
      (define (view-controller-go drscheme-frame program-expander)
        
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
                
                (define continue-semaphore #f)
                
                (define (update-view/next-step new-view)
                  (set! view-currently-updating new-view)
                  (semaphore-post continue-semaphore))
                
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
                
                ; receive-result takes a result from the model and renders it on-screen
                ; : (step-result semaphore -> void)
                (define (receive-result result continue-user-computation-semaphore)
                  (fprintf (current-error-port) "in receive-result with result: ~a" result)
                  (set! continue-semaphore continue-user-computation-semaphore)
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
                    (update-view view-currently-updating))))
          
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
          (model:go program-expander receive-result)
          (send s-frame show #t)))
  
      (define beginner-level-name-symbol 'beginner)
      (define intermediate-level-name-symbol 'intermediate)
      
      (define stepper-bitmap
        (drscheme:unit:make-bitmap
         "Step"
         (build-path (collection-path "icons") "foot.bmp")))
      
      (drscheme:get/extend:extend-unit-frame
       (lambda (super%)
         (class super% 
           
           (inherit get-button-panel get-interactions-text get-definitions-text)
           (rename [super-disable-evaluation disable-evaluation]
                   [super-enable-evaluation enable-evaluation])
           (public stepper-frame)
           
           (override enable-evaluation disable-evaluation)
           
           (super-instantiate ())
           
           (define program-expander
             (contract
              (-> (-> void?) ; init
                  (-> string? any? void?)
                  (-> (union eof-object? syntax? (cons/p string? any?)) (-> void?) void?) ; iter
                  void?)
              (lambda (init error iter)
                (drscheme:eval:expand-program
                 (drscheme:language:make-text/pos (get-definitions-text) 
                                                  0
                                                  (send (get-definitions-text)
                                                        last-position)) 
                 (frame:preferences:get (drscheme:language-configuration:get-settings-preferences-symbol))
                 init
                 error
                 void ; kill
                 iter))
              'program-expander
              'caller))
           
           (define stepper-button 
             (make-object button%
               (stepper-bitmap this)
               (get-button-panel)
               (lambda (button evt)
                 (let ([language-level (drscheme:language-configuration:get-settings-preferences-symbol)])
                   (if #t ; (or (eq? language-level beginner-level-name-symbol)
                          ;  (eq? language-level intermediate-level-name-symbol))
                       (view-controller-go this program-expander)
                       (message-box "Stepper" 
                                    (format (string-append "Language level is set to \"~a\".~n"
                                                           "The stepper only works for the \"~a\" and the~n"
                                                           "\"~a\" language levels.~n")
                                            language-level
                                            beginner-level-name-symbol
                                            intermediate-level-name-symbol)
                                    #f 
                                    '(ok)))))))
           
           (define (enable-evaluation)
             (send stepper-button enable #t)
             (super-enable-evaluation))
           
           (define (disable-evaluation)
             (send stepper-button enable #f)
             (super-disable-evaluation))
           
           (define frame-internal #f)
           
           (define stepper-frame
               (case-lambda
                 (() frame-internal)
                 ((new-val) (set! frame-internal new-val))))
           
           (send (get-button-panel) change-children
                 (lambda (l)
                   (cons stepper-button (remq stepper-button l))))))))))