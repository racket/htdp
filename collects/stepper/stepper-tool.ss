(module stepper-tool mzscheme
  (require (lib "contract.ss")
           (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred")  
           (prefix frame: (lib "framework.ss" "framework"))
           (lib "unitsig.ss")
           (lib "class.ss")
           (lib "etc.ss")
           (lib "list.ss")
           (prefix model: "private/model.ss")
	   "private/my-macros.ss"
           (prefix x: "private/mred-extensions.ss")
           "private/shared.ss"
           "private/model-settings.ss"
           (lib "pconvert.ss")
           (lib "string-constant.ss" "string-constants")
           (lib "async-channel.ss"))

  ;; mflatt: MINOR HACK - work around temporary
  ;;         print-convert problems
  (define (stepper-print-convert v)
    (or (and (procedure? v)
	     (object-name v))
	(print-convert v)))
    
  
  ; have to define this as a macro because string-constant is a macro
  (define-syntax string-constant-list
    (lambda (stx)
      (syntax-case stx ()
        [(_ (constant-name ...))
         #`(list (string-constant constant-name) ...)])))
  
  ; hidden invariant: this list should be a sublist of the language-level dialog (i.e., same order):
  (define stepper-works-for
    (string-constant-list
     (beginning-student
      beginning-student/abbrev
      intermediate-student
      intermediate-student/lambda
      )))
  
  (provide tool@)
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define (phase1) (void))
      (define (phase2) (void))
      
      (define stepper-initial-width 500)
      (define stepper-initial-height 500)
      
      (define image? x:image?)
      
      (define drscheme-eventspace (current-eventspace))
      
      (define stepper-frame%
        (class (drscheme:frame:basics-mixin (frame:frame:standard-menus-mixin frame:frame:basic%))
          
          (init-field drscheme-frame)
          (rename [super-on-close on-close])
          (public set-printing-proc)
          
          (define (set-printing-proc proc)
            (set! printing-proc proc))
          
          (define (printing-proc item evt)
            (message-box "error?" "shouldn't be called"))
          
          (define (file-menu:print a b) (printing-proc a b))
          
          ;; CUSTODIAN:
          
          (define custodian #f)
          (define/public (set-custodian! cust)
            (set! custodian cust))
          
          ;; WARNING BOXES:
          
          (define program-changed-warning-str (string-constant stepper-program-has-changed))
          (define window-closed-warning-str (string-constant stepper-program-window-closed))
          
          (define warning-message-visible-already #f)
          (define (add-warning-message warning-str)
            (let ([warning-msg (instantiate x:stepper-warning% () 
                                 (warning-str warning-str)
                                 (parent (get-area-container)))])
              (send (get-area-container)
                    change-children
                    (if warning-message-visible-already
                        (lambda (l) 
                          (list (car l)
                                warning-msg
                                (caddr l)))
                        (lambda (l)
                          (list (car l)
                                warning-msg
                                (cadr l)))))
              (set! warning-message-visible-already #t)))
          
          (inherit get-area-container)
          (define program-change-already-warned? #f)
          (define/public (original-program-changed)
            (unless program-change-already-warned?
              (set! program-change-already-warned? #t)
              (add-warning-message program-changed-warning-str)))
          
          (define/public (original-program-gone)
            (add-warning-message window-closed-warning-str))
          
          
          (override  on-close) ; file-menu:print
          (define (on-close)
            (when custodian
              (custodian-shutdown-all custodian))
            (send drscheme-frame on-stepper-close)
            (super-on-close))
          
          (super-instantiate ("Stepper" #f stepper-initial-width stepper-initial-height))))
      
      
      (define (view-controller-go drscheme-frame program-expander)
        
        (define settings 
          (frame:preferences:get (drscheme:language-configuration:get-settings-preferences-symbol)))
        (define language
          (drscheme:language-configuration:language-settings-language settings))
        (define language-level-name
          (car (last-pair (send language get-language-position))))
        (define simple-settings
          (drscheme:language-configuration:language-settings-settings settings))
        
        ;; VALUE CONVERSION CODE:
        
        ;; render-to-string : TST -> string
        (define (render-to-string val)
          (let ([string-port (open-output-string)])
            (send language
                  render-value
                  val
                  simple-settings
                  string-port
                  #f)
            (get-output-string string-port)))
        
        ;; make-print-convert-hook: simple-settings -> (TST (TST -> TST) (TST -> TST) -> TST)
        ;; this code copied from various locations in language.ss and rep.ss
        (define (make-print-convert-hook simple-settings)
          (lambda (expr basic-convert sub-convert)
            (cond
              [(is-a? expr snip%) 
               (send expr copy)]
              [((drscheme:rep:use-number-snip) expr)
               (let ([number-snip-type (drscheme:language:simple-settings-fraction-style simple-settings)])
                 (cond
                   [(eq? number-snip-type 'repeating-decimal)
                    (drscheme:number-snip:make-repeating-decimal-snip expr #f)]
                   [(eq? number-snip-type 'repeating-decimal-e)
                    (drscheme:number-snip:make-repeating-decimal-snip expr #t)]
                   [(eq? number-snip-type 'mixed-fraction)
                    (drscheme:number-snip:make-fraction-snip expr #f)]
                   [(eq? number-snip-type 'mixed-fraction-e)
                    (drscheme:number-snip:make-fraction-snip expr #t)]
                   [else
                    (error 'which-number-snip
                           "expected either 'repeating-decimal, 'repeating-decimal-e, 'mixed-fraction, or 'mixed-fraction-e got : ~e"
                           number-snip-type)]))]
              [else (basic-convert expr)])))
        
        (define drscheme-inspector (current-inspector))
        
        ;; render-to-sexp : TST -> sexp
        (define (render-to-sexp val)
          (parameterize ([current-print-convert-hook (make-print-convert-hook simple-settings)])
            (set-print-settings
             language
             simple-settings
             (lambda () 
               (simple-module-based-language-convert-value val simple-settings)))))
        
        ; channel for incoming views
        (define view-channel (make-async-channel))
        
        ; the semaphore associated with the view at the end of the view-history
        ; note that because these are fresh semaphores for every step, posting to a semaphore
        ; multiple times is no problem.
        (define release-for-next-step #f)
        
        ; the list of available views
        (define view-history null)
        
        ; the view in the stepper window
        (define view 0)
        
        ; whether the stepper is waiting for a new view to become available
        ; (initially true)
        (define stepper-is-waiting? #t)
        
        (define never-step-again #f)
        
        ; deadlock is prevented by only running code which updates these variables in the drscheme-eventspace
        
        (define (try-to-get-view-pair)
          (when stepper-is-waiting?
            (error 'try-to-get-view "try-to-get-view should not be reachable when already waiting for new step"))
          (let ([try-get (async-channel-try-get view-channel)])
            (if try-get
                try-get
                (begin
                  (set! stepper-is-waiting? #t)
                  #f))))
        
        ; hand-off-and-block : (-> text%? boolean? void?)
        ; hand-off-and-block generates a new semaphore, hands off a thunk to drscheme's eventspace,
        ; and blocks on the new semaphore.  The thunk adds the text% to the waiting queue, and checks
        ; to see if the stepper is waiting for a new step.  If so, it takes that new text% out of 
        ; the queue, puts it on the list of available ones, and updates the view.  The new semaphore
        ; won't be released until the user clicks next on the last step.
        
        (define (hand-off-and-block step-text end-of-stepping?)
          (let ([new-semaphore (make-semaphore)])
            (parameterize ([current-eventspace drscheme-eventspace])
              (queue-callback
               (lambda ()
                 (when end-of-stepping?
                   (set! never-step-again #t))
                 (async-channel-put view-channel (list step-text new-semaphore))
                 (when stepper-is-waiting?
                   (let ([try-get (async-channel-try-get view-channel)])
                     (unless try-get
                       (error 'check-for-stepper-waiting "queue is empty, even though a step was just added."))
                     (set! stepper-is-waiting? #f)
                     (add-view-pair try-get)
                     (update-view/existing (- (length view-history) 1))))))
              (semaphore-wait new-semaphore))))
        
        (define (add-view-pair view-pair)
          (set! release-for-next-step (cadr view-pair))
          (set! view-history (append view-history (list (car view-pair)))))
        
        ; build gui object:
        
        (define (home)
          (when stepper-is-waiting?
            (set! stepper-is-waiting? #f))
          (update-view 0))
        
        (define (next)
          (update-view (+ view 1)))
        
        ; make this into a special last step
        ;(message-box "Stepper"
        ;             (string-append
        ;              "The source text for this program has changed or is no longer "
        ;              "available.  No further steps can be computed."))
        
        (define (previous)
          (when stepper-is-waiting?
            (set! stepper-is-waiting? #f))
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
        
        ; update-view : (-> number? void?) : display the step with the given number.  If the 
        ; given step is one past the end of the list, release the semaphore associated with 
        ; the last step.  Try to get a new view.  If one is not available, simply return
        ; (all buttons should still be disabled).  If one is available, tack it on to the end
        ; of the list and display it.
        
        (define (update-view new-view)
          (if (= new-view (length view-history))
              (begin
                (unless never-step-again
                  (semaphore-post release-for-next-step)
                  (let ([try-get (try-to-get-view-pair)])
                    (if try-get
                        (begin 
                          (add-view-pair try-get)
                          (update-view/existing new-view))
                        (en/dis-able-buttons)))))
              (update-view/existing new-view)))
        
        (define (update-view/existing new-view)
          (set! view new-view)                  
          (let ([e (list-ref view-history view)])
            (send e begin-edit-sequence)
            (send canvas set-editor e)
            (send e reset-width canvas)
            (send e set-position (send e last-position))
            (send e end-edit-sequence))
          (en/dis-able-buttons))
        
        (define (en/dis-able-buttons)
          (send previous-button enable (not (zero? view)))
          (send home-button enable (not (zero? view)))
          (send next-button enable (not (or stepper-is-waiting? (and never-step-again (= view (- (length view-history) 1)))))))
        
        (define (print-current-view item evt)
          (send (send canvas get-editor) print))
        
        (define (final-step? result)
          #f
          ;(ormap (lambda (fn) (fn result)) (list before-error-result? error-result? finished-result?))
          )
        
        ; receive-result takes a result from the model and renders it on-screen
        ; : (step-result -> void)
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
                        (make-object x:stepper-text%
                          (before-error-result-finished-exprs result)
                          (before-error-result-exp result)
                          (before-error-result-redex result)
                          null
                          null
                          (before-error-result-err-msg result)
                          (before-error-result-after-exprs result))]
                       [(error-result? result)
                        (make-object x:stepper-text%
                          (error-result-finished-exprs result)
                          null
                          null
                          null
                          null
                          (error-result-err-msg result)
                          null)]
                       [(finished-result? result)
                        (make-object x:stepper-text%
                          (finished-result-finished-exprs result)
                          null
                          null
                          null
                          null
                          #f
                          null)]
                       [(finished-stepping? result)
                        x:finished-text])])
            (hand-off-and-block step-text (final-step? result))))
        
        ; need to capture the custodian as the thread starts up:
        (define (program-expander-prime init iter)
          (program-expander (lambda args
                              (send s-frame set-custodian! (current-custodian))
                              (apply init args))
                            iter))
        
        (send s-frame set-printing-proc print-current-view)
        (send button-panel stretchable-width #f)
        (send button-panel stretchable-height #f)
        (send canvas stretchable-height #t)
        (en/dis-able-buttons)
        (send (send s-frame edit-menu:get-undo-item) enable #f)
        (send (send s-frame edit-menu:get-redo-item) enable #f)
        (model:go program-expander-prime receive-result (get-render-settings render-to-string render-to-sexp #t)
                  (not (string=? language-level-name (string-constant intermediate-student/lambda))))
        (send s-frame show #t)
        
        s-frame)
  
      (define beginner-level-name-symbol 'beginner)
      (define intermediate-level-name-symbol 'intermediate)
      
      (define stepper-bitmap
        (drscheme:unit:make-bitmap
         "Step"
         (build-path (collection-path "icons") "foot.bmp")))

      (define stepper-unit-frame<%>
        (interface ()
          get-stepper-frame
          on-stepper-close))
      
      (define (stepper-unit-frame-mixin super%)
        (class* super% (stepper-unit-frame<%>)
          
          (inherit get-button-panel get-interactions-text get-definitions-text)
          (rename [super-on-close on-close])
          
          (define stepper-frame #f)
          (define/public (on-stepper-close)
            (set! stepper-frame #f))
          (define/public (get-stepper-frame) stepper-frame)
          
          (super-instantiate ())
          
          (define program-expander
            (contract
             (-> (-> void?) ; init
                 (-> (union eof-object? syntax? (cons/p string? any?)) (-> void?) void?) ; iter
                 void?)
             (lambda (init iter)
               (let* ([lang-settings 
                       (frame:preferences:get
                        (drscheme:language-configuration:get-settings-preferences-symbol))]
                      [lang (drscheme:language-configuration:language-settings-language lang-settings)]
                      [settings (drscheme:language-configuration:language-settings-settings lang-settings)])
                 (drscheme:eval:expand-program
                  (drscheme:language:make-text/pos (get-definitions-text) 
                                                   0
                                                   (send (get-definitions-text)
                                                         last-position)) 
                  lang-settings
                  #f
                  (lambda ()
                    (init)
                    (error-value->string-handler
                     (lambda (val len)
                       (let ([sp (open-output-string)])
                         (send lang render-value val settings sp #f)
                         (let ([str (get-output-string sp)])
                           (if ((string-length str) . <= . len)
                               str
                               (string-append (substring str 0 (max 0 (- len 3))) "..."))))))
                    (drscheme:teachpack:install-teachpacks 
                     (frame:preferences:get 'drscheme:teachpacks))) ; this belongs in model, but I'd need a unit rewrite
                  void ; kill
                  iter)))
             'program-expander
             'caller))
          
          (define/public (get-stepper-button) stepper-button)
          (define stepper-button 
            (make-object button%
              (stepper-bitmap this)
              (get-button-panel)
              (lambda (button evt)
                (if stepper-frame
                    (send stepper-frame show #t)
                    (let* ([settings (frame:preferences:get (drscheme:language-configuration:get-settings-preferences-symbol))]
                           [language (drscheme:language-configuration:language-settings-language settings)]
                           [language-level (car (last-pair (send language get-language-position)))])
                      (if (member language-level stepper-works-for)
                          (set! stepper-frame (view-controller-go this program-expander))
                          (message-box "Stepper"
                                       (string-append
                                        "The language level is set to \"" language-level "\". "
                                        "Currently, the stepper works only for the \"" (car stepper-works-for)
                                        "\" through the \"" (car (reverse stepper-works-for)) "\" language levels."))))))))
          
          (rename [super-enable-evaluation enable-evaluation])
          (define/override (enable-evaluation)
            (send stepper-button enable #t)
            (super-enable-evaluation))
          
          (rename [super-disable-evaluation disable-evaluation])
          (define/override (disable-evaluation)
            (send stepper-button enable #f)
            (super-disable-evaluation))
          
          (define/override (on-close)
            (when stepper-frame
              (send stepper-frame original-program-gone))
            (super-on-close))
          
          (send (get-button-panel) change-children
                (lx (cons stepper-button (remq stepper-button _))))))
      

      (define (stepper-definitions-text-mixin %)
        (class %
          
          (inherit get-top-level-window)
          (define/private (notify-stepper-frame-of-change)
            (let ([win (get-top-level-window)])
              (when (is-a? win stepper-unit-frame<%>) ;; should only be #f when win is #f.
                (let ([stepper-window (send win get-stepper-frame)])
                  (when stepper-window
                    (send stepper-window original-program-changed))))))
          
          (rename [super-on-insert on-insert])
          (define/override (on-insert x y)
            (super-on-insert x y)
            (notify-stepper-frame-of-change))
          
          (rename [super-on-delete on-delete])
          (define/override (on-delete x y)
            (super-on-delete x y)
            (notify-stepper-frame-of-change))
          
          (super-instantiate ())))
      
      ;; COPIED FROM drscheme/private/language.ss
      ;; simple-module-based-language-convert-value : TST settings -> TST
      (define (simple-module-based-language-convert-value value simple-settings)
	(case (drscheme:language:simple-settings-printing-style simple-settings)
	  [(write) value]
	  [(constructor)
	   (parameterize ([constructor-style-printing #t]
			  [show-sharing (drscheme:language:simple-settings-show-sharing simple-settings)])
			 (stepper-print-convert value))]
	  [(quasiquote)
	   (parameterize ([constructor-style-printing #f]
			  [show-sharing (drscheme:language:simple-settings-show-sharing simple-settings)])
			 (stepper-print-convert value))]))
      
      ;; set-print-settings ; settings ( -> TST) -> TST
      (define (set-print-settings language simple-settings thunk)
        (unless (method-in-interface? 'set-printing-parameters (object-interface language))
          (error 'stepper-tool "language object does not contain set-printing-parameters method"))
        (send language set-printing-parameters simple-settings thunk))
      
      (drscheme:get/extend:extend-unit-frame stepper-unit-frame-mixin)
      (drscheme:get/extend:extend-definitions-text stepper-definitions-text-mixin))))
