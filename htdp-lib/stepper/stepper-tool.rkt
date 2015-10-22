#lang racket/unit

(require racket/class
         drracket/tool
         mred
         (prefix-in frame: framework)
         mrlib/switchable-button
         mzlib/pconvert
         racket/pretty
         racket/match
         string-constants
         lang/stepper-language-interface
         (prefix-in x: "private/mred-extensions.rkt")
         "private/shared.rkt"
         "private/xml-sig.rkt"
         "drracket-button.rkt") ;; get the stepper-button-callback private-member-name

(import drracket:tool^ xml^ view-controller^)
(export drracket:tool-exports^ stepper-frame^)

(define-logger stepper)

;; tool magic here:
(define (phase1)
  ;; experiment with extending the language... parameter-like fields for stepper parameters
  (drracket:language:extend-language-interface
   stepper-language<%>
   (lambda (superclass)
     (class* superclass (stepper-language<%>)
       (public stepper:supported?)
       (define (stepper:supported?) #f)

       (public stepper:enable-let-lifting?)
       (define (stepper:enable-let-lifting?) #f)

       ;; these next three parameters should be overridden by
       ;; the language definition to match the way that the language
       ;; wants these values printed.
       (public stepper:show-lambdas-as-lambdas?)
       (define (stepper:show-lambdas-as-lambdas?) #t)

       (public stepper:show-inexactness?)
       (define (stepper:show-inexactness?) #t)

       (public stepper:print-boolean-long-form?)
       (define (stepper:print-boolean-long-form?) #t)

       (public stepper:show-consumed-and/or-clauses?)
       (define (stepper:show-consumed-and/or-clauses?) #t)

       (public stepper:render-to-sexp)
       (define (stepper:render-to-sexp val settings language-level)
         (when (boolean? val)
           (log-stepper-debug "render-to-sexp got a boolean: ~v\n" val))
         (or (and (procedure? val)
                  (object-name val))
             (parameterize ([pretty-print-show-inexactness (stepper:show-inexactness?)]
                            [current-print-convert-hook stepper-print-convert-hook])
               (call-with-values
                (lambda ()
                  ;; I'm not sure that these print settings actually need to be set...
                  ;; or... that they need to be set *here*. They might need to be set
                  ;; when the pretty-print to the actual width occurs. That is, when
                  ;; they get converted to strings...
                  ;; try removing this wrapper when things are working. (2015-10-22)
                  (call-with-print-settings
                   language-level
                   settings
                   (lambda ()
                     (drracket:language:simple-module-based-language-convert-value
                      val
                      settings))))
                (lambda args
                  (match args
                    [(list value should-be-written?)
                     (cond [should-be-written?
                            ;; warning, don't know if this happens in the stepper:
                            (log-stepper-debug "print-convert returned writable: ~v\n" value)
                            value]
                           [else
                            (let ([ans (let ([os-port (open-output-string)])
                                         (print value os-port)
                                         (when (boolean? val)
                                           (log-stepper-debug "string printed by print: ~v\n" (get-output-string os-port)))
                                         ;; this 'read' is somewhat scary. I'd like to
                                         ;; get rid of this:
                                         (read (open-input-string (get-output-string os-port))))])
                              (log-stepper-debug "print-convert returned string that read mapped to: ~s\n" ans)
                              ans)])]
                    [(list value)
                     (log-stepper-debug "render-to-sexp: value returned from convert-value: ~v\n" value)
                     value]))))))

       (super-instantiate ())))))

(define (phase2) (void))

;; this should be a preference:
(define stepper-initial-width 500)
(define stepper-initial-height 500)

(define (extract-language-level definitions-text)
  (settings->language-level (definitions-text->settings definitions-text)))

(define (definitions-text->settings definitions-text)
  (send definitions-text get-next-settings))

(define (settings->language-level settings)
  (drracket:language-configuration:language-settings-language settings))

(define (stepper-works-for? language-level)
  (or (send language-level stepper:supported?)
      (getenv "PLTSTEPPERUNSAFE")))

;; the stepper's frame:

(define stepper-frame%
  (class (drracket:frame:basics-mixin
          (frame:frame:standard-menus-mixin frame:frame:basic%))

    (init-field drracket-tab)

    ;; PRINTING-PROC
    ;; I frankly don't think that printing (i.e., to a printer) works
    ;; correctly. 2005-07-01, JBC
    (public set-printing-proc)

    (define (set-printing-proc proc)
      (set! printing-proc proc))

    (define (printing-proc item evt)
      (message-box "error?" "shouldn't be called"))

    (define/private (file-menu:print a b) (printing-proc a b))

    ;; MENUS

    (define/override (edit-menu:between-find-and-preferences edit-menu)
      (void))
    (define/override (edit-menu:between-select-all-and-find edit-menu)
      (void))
    (define/override (file-menu:between-save-as-and-print file-menu)
      (void))

    ;; CUSTODIANS
    ;; The custodian is used to halt the stepped computation when the
    ;; stepper window closes.  The custodian is captured when the stepped
    ;; computation starts.

    (define custodian #f)
    (define/public (set-custodian! cust)
      (set! custodian cust))
    (define/augment (on-close)
      (when custodian
        (custodian-shutdown-all custodian))
      (send drracket-tab on-stepper-close)
      (inner (void) on-close))

    ;; WARNING BOXES:

    (define program-changed-warning-str
      (string-constant stepper-program-has-changed))
    (define window-closed-warning-str
      (string-constant stepper-program-window-closed))

    (define warning-message-visible-already #f)
    (define/private (add-warning-message warning-str)
      (let ([warning-msg (new x:stepper-warning%
                              [warning-str warning-str]
                              [parent (get-area-container)])])
        (send (get-area-container)
              change-children
              (if warning-message-visible-already
                  (lambda (l)
                    (list (car l) warning-msg (caddr l)))
                  (lambda (l)
                    (list (car l) warning-msg (cadr l)))))
        (set! warning-message-visible-already #t)))

    (inherit get-area-container)
    (define program-change-already-warned? #f)
    (define/public (original-program-changed)
      (unless program-change-already-warned?
        (set! program-change-already-warned? #t)
        (add-warning-message program-changed-warning-str)))

    (define/public (original-program-gone)
      (add-warning-message window-closed-warning-str))

    (super-new [label "Stepper"] [parent #f]
               [width stepper-initial-width]
               [height stepper-initial-height])))


;; stepper-unit-frame<%> : the interface that the extended drracket frame
;; fulfils
(define stepper-tab<%>
  (interface ()
    get-stepper-frame
    on-stepper-close))

;; stepper-unit-frame-mixin : the mixin that is applied to the drracket
;; frame to interact with a possible stepper window. Specifically, this
;; mixin needs to manage the creation and visibility of the stepper button.
(define (stepper-unit-frame-mixin super%)
  (class* super% ()
    (inherit get-button-panel register-toolbar-button get-current-tab get-tabs)

    (super-new)

    ;; STEPPER BUTTON

    (define/public (get-stepper-button) stepper-button)

    (define stepper-button-parent-panel
      (new frame:panel:horizontal-discrete-sizes%
           [parent (get-button-panel)]
           [stretchable-width #f]
           [stretchable-height #f]))

    (define stepper-button
      (new switchable-button%
           [parent stepper-button-parent-panel]
           [label (string-constant stepper-button-label)]
           [bitmap x:step-img]
           [callback (lambda (dont-care) (send (get-current-tab)
                                               stepper-button-callback))]))

    (register-toolbar-button stepper-button #:number 59)

    (define (stepper-button-show)
      (unless (send stepper-button is-shown?)
        (send (send stepper-button get-parent)
              add-child stepper-button)))

    (define (stepper-button-hide)
      (when (send stepper-button is-shown?)
        (send (send stepper-button get-parent)
              delete-child stepper-button)))

    ;; when the window closes, notify all of the stepper frames.
    (define/augment (on-close)
      (for ([tab (in-list (get-tabs))])
        (define possible-stepper-frame (send tab get-stepper-frame))
        (when possible-stepper-frame
          (send possible-stepper-frame original-program-gone)))
      (inner (void) on-close))

    ;; when we change tabs, show or hide the stepper button.
    (define/augment (on-tab-change old new)
      (show/hide-stepper-button)
      (inner (void) on-tab-change old new))

    ;; add the stepper button to the button panel:
    (send (get-button-panel) change-children
          (lambda (x)
            (cons stepper-button-parent-panel
                  (remq stepper-button-parent-panel x))))

    ;; show or hide the stepper button depending
    ;; on the language level
    (define/public (show/hide-stepper-button)
      (cond [(send (get-current-tab) current-lang-supports-stepper?)
             (stepper-button-show)]
            [else
             (stepper-button-hide)]))

    ;; hide stepper button if it's not supported for the initial language:
    (show/hide-stepper-button)))

;; stepper-tab-mixin : the mixin that is applied to drracket tabs, to
;; interact with a possible stepper window.
(define (stepper-tab-mixin super%)
  (class* super% (stepper-tab<%>)

    (inherit get-ints get-defs get-frame get-directory)

    ;; a reference to a possible stepper frame.
    (define stepper-frame #f)
    (define/public (on-stepper-close)
      (set! stepper-frame #f))
    (define/public (get-stepper-frame) stepper-frame)

    (super-new)

    ;; program-expander : produces expanded expressions from the
    ;; definitions window one at a time and calls 'iter' on each one
    (define (program-expander init iter)
      (define lang-settings (send (get-defs) get-next-settings))
      (define lang (drracket:language-configuration:language-settings-language lang-settings))
      (define settings (drracket:language-configuration:language-settings-settings lang-settings))
      (drracket:eval:expand-program
       (drracket:language:make-text/pos
        (get-defs)
        0
        (send (get-defs) last-position))
       lang-settings
       #f
       (lambda ()
         (init)
         (error-value->string-handler
          (lambda (val len)
            (let ([sp (open-output-string)])
              (send lang render-value val settings sp)
              (let ([str (get-output-string sp)])
                (if ((string-length str) . <= . len)
                    str
                    (string-append (substring str 0 (max 0 (- len 3)))
                                   "..."))))))
         (current-print void))
       void ; kill
       iter))

    ;; a thunk which calls the language's front-end/finished-complete-program thunk
    ;; to trigger a dynamic-require of the user's module
    (define (dynamic-requirer)
      (define lang-settings (send (get-defs) get-next-settings))
      (define lang (drracket:language-configuration:language-settings-language lang-settings))
      (define settings (drracket:language-configuration:language-settings-settings lang-settings))
      ;; copied from rep.rkt... how we actually dyn-req the program:
      (call-with-continuation-prompt
       (λ ()
         (send lang front-end/finished-complete-program settings)
         ;; not sure we need this. Hope not....
         #;(call-with-break-parameterization
            user-break-parameterization
            (λ ()
              (send lang front-end/finished-complete-program settings))))
       (default-continuation-prompt-tag)
       (λ args (void))))


    ;; called from drracket-button.rkt, installed via the #lang htdp/bsl (& co) reader into drracket
    (define/public (stepper-button-callback)
      (cond
        [stepper-frame (send stepper-frame show #t)]
        [else (create-new-stepper)]))

    ;; open a new stepper window, start it running
    (define (create-new-stepper)
      (let* ([language-level
              (extract-language-level (get-defs))]
             [language-level-name (language-level->name language-level)])
        (if (or (stepper-works-for? language-level)
                (is-a? language-level drracket:module-language:module-language<%>))
            (parameterize ([current-directory (or (get-directory) (current-directory))])
              (set! stepper-frame
                    (vc-go this
                           program-expander
                           dynamic-requirer
                           (+ 1 (send (get-defs) get-start-position))
                           (+ 1 (send (get-defs) get-end-position)))))
            (message-box
             (string-constant stepper-name)
             (format (string-constant stepper-language-level-message)
                     language-level-name)))))

    (define/override (enable-evaluation)
      (super enable-evaluation)
      (send (send (get-frame) get-stepper-button) enable #t))

    (define/override (disable-evaluation)
      (super disable-evaluation)
      (send (send (get-frame) get-stepper-button) enable #f))

    (define/public (current-lang-supports-stepper?)
      (stepper-works-for? (extract-language-level (get-defs))))

    (define/public (notify-stepper-frame-of-change)
      (when stepper-frame
        (send stepper-frame original-program-changed)))

    (define/augment (on-close)
      (when stepper-frame
        (send stepper-frame original-program-gone))
      (inner (void) on-close))

    ))



;; stepper-definitions-text-mixin : a mixin for the definitions text that
;; alerts thet stepper when the definitions text is altered or destroyed
(define (stepper-definitions-text-mixin %)
  (class %

    (inherit get-tab get-top-level-window)

    (define/augment (on-insert x y)
      (unless metadata-changing-now?
        (send (get-tab) notify-stepper-frame-of-change))
      (inner (void) on-insert x y))

    (define/augment (on-delete x y)
      (unless metadata-changing-now?
        (send (get-tab) notify-stepper-frame-of-change))
      (inner (void) on-delete x y))

    (define/augment (after-set-next-settings s)
      (let ([tlw (get-top-level-window)])
        (when tlw
          (send tlw show/hide-stepper-button)))
      (inner (void) after-set-next-settings s))

    (define metadata-changing-now? #f)

    ;; don't pay attention to changes that occur on metadata.
    ;; this assumes that metadata changes cannot be nested.
    (define/augment (begin-metadata-changes)
      (set! metadata-changing-now? #t)
      (inner (void) begin-metadata-changes))

    (define/augment (end-metadata-changes)
      (set! metadata-changing-now? #f)
      (inner (void) end-metadata-changes))

    (super-new)))

;; apply the mixins dynamically to the drracket unit frame and
;; definitions text:
(drracket:get/extend:extend-unit-frame stepper-unit-frame-mixin)
(drracket:get/extend:extend-definitions-text stepper-definitions-text-mixin)
(drracket:get/extend:extend-tab stepper-tab-mixin)

;; set-print-settings ; settings ( -> TST) -> TST
(define (call-with-print-settings language simple-settings thunk)
  ;; I have to say, I'm somewhat alarmed by the presence of the pretty-print
  ;; width here. I'm hoping it doesn't affect the thunks that we're using here...
  #;((drracket:language:make-setup-printing-parameters) thunk simple-settings 'infinity)
  ;; okay, this is getting interesting. I'm updating this code for 2015, and ...
  ;; things are a little scary. In particular, it looks like 'set-printing-parameters
  ;; is no longer the name of this method... this kind of dynamic dispatch is frightening.
  (if (method-in-interface? 'set-printing-parameters (object-interface language))
      (send language set-printing-parameters simple-settings thunk)
      ;; assume that the current print-convert context is fine
      (error 'stepper-tool "language object does not contain set-printing-parameters method")
      ;; 2009-09-11, JBC : Gee Whiz, why the heck is it okay to assume that !?
      #;(thunk)))

;; WE REALLY WANT TO GET RID OF THIS STUFF (2005-07-01, JBC)

;; stepper-convert-hook:
;;   (TST (TST -> TST) (TST -> TST) -> TST)
;; this code copied from various locations in language.rkt and rep.rkt
(define (stepper-print-convert-hook exp basic-convert sub-convert)
  (cond
    [(is-a? exp snip%)
     (send exp copy)]
    #;
    [((drracket:rep:use-number-snip) exp)
     (let ([number-snip-type
            (drracket:language:simple-settings-fraction-style
             simple-settings)])
       (cond
         [(eq? number-snip-type 'repeating-decimal)
          (drracket:number-snip:make-repeating-decimal-snip exp #f)]
         [(eq? number-snip-type 'repeating-decimal-e)
          (drracket:number-snip:make-repeating-decimal-snip exp #t)]
         [(eq? number-snip-type 'mixed-fraction)
          (drracket:number-snip:make-fraction-snip exp #f)]
         [(eq? number-snip-type 'mixed-fraction-e)
          (drracket:number-snip:make-fraction-snip exp #t)]
         [else
          (error 'which-number-snip
                 "expected either 'repeating-decimal, 'repeating-decimal-e, 'mixed-fraction, or 'mixed-fraction-e got : ~e"
                 number-snip-type)]))]
    [else (basic-convert exp)]))
