(unit/sig stepper:settings^
  (import [z : zodiac:system^]
          [d : drscheme:export^]
          [p : mzlib:print-convert^]
          [e : stepper:error^]
          [a : stepper:annotate^]
          [r : stepper:reconstruct^]
          stepper:shared^
          ;;; must also import text and settings)
  
  (define beginner-level-name "Beginning Student")
  
  (define (send-to-other-eventspace eventspace thunk)
    (parameterize ([current-eventspace eventspace])
      (queue-callback thunk)))
       

  (define drscheme-eventspace (current-eventspace))

  (define (send-to-drscheme-eventspace thunk)
    (send-to-other-eventspace drscheme-eventspace thunk))
  
  (define par-constructor-style-printing #f)
  (define (constructor-style-printing?)
    par-constructor-style-printing)
  
  (define par-abbreviate-cons-as-list #f)
  (define (abbreviate-cons-as-list?)
    par-abbreviate-cons-as-list)
  
  (define par-cons #f)
  (define (user-cons? val)
    (eq? val par-cons))
  
  (define par-vector #f)
  (define (user-vector? val)
    (eq? val par-vector))
  
  (define user-pre-defined-vars #f)
  
  (define (check-pre-defined-var identifier)
    (memq identifier user-pre-defined-vars))
  
  (define user-namespace #f)

  (define (check-global-defined identifier)
    (with-handlers
        ([exn:variable? (lambda args #f)])
      (global-lookup identifier)
      #t))
  
  (define (global-lookup identifier)
    (parameterize ([current-namespace user-namespace])
      (global-defined-value identifier)))
    
  (define (image? val)
   (is-a? val snip%))
  
  (define stepper-semaphore (make-semaphore))


  (define finished-exprs #f)

  (define current-expr #f)
  (define packaged-envs a:initial-env-package)
         
  (define user-eventspace (make-eventspace))
         
  (define (send-to-user-eventspace thunk)
    (send-to-other-eventspace user-eventspace thunk))
         
  (define user-primitive-eval #f)
  (define user-vocabulary #f)
         
  (define reader 
    (z:read (f:gui-utils:read-snips/chars-from-buffer text)
            (z:make-location 1 1 0 "stepper-text")))
  
  (send-to-user-eventspace 
   (lambda ()
     (set! user-primitive-eval (current-eval))
     (d:basis:initialize-parameters (make-custodian) settings)
     (d:rep:invoke-library)
     (set! user-namespace (current-namespace))
     (set! user-pre-defined-vars (map car (make-global-value-list)))
     (set! user-vocabulary (d:basis:current-vocabulary))
     (set! par-constructor-style-printing (p:constructor-style-printing))
     (set! par-abbreviate-cons-as-list (p:abbreviate-cons-as-list))
     (set! par-cons (global-defined-value 'cons))
     (set! par-vector (global-defined-value 'vector))
     (p:current-print-convert-hook 
      (lambda (v basic-convert sub-convert)
        (if (image? v)
            v
            (basic-convert v))))
     (semaphore-post stepper-semaphore)))
  
  (semaphore-wait stepper-semaphore))

  (define terminate-user-thread-continuation #f)
  (send-to-user-eventspace
   (lambda ()
     (let/ec k
       (set! terminate-user-thread-continuation k)
       (semaphore-post stepper-semaphore))))
  
  (semaphore-wait stepper-semaphore)

  (define print-convert
    (let ([print-convert-result 'not-a-real-value])    
      (lambda (val)
        (send-to-user-eventspace
         (lambda ()
           (set! print-convert-result
                 (p:print-convert val))
           (semaphore-post stepper-semaphore)))
        (semaphore-wait stepper-semaphore)
        print-convert-result)))
  
  (define (read-next-expr)
    (send-to-user-eventspace
     (lambda ()
       (let/ec k
         (let ([inner-handler
                (lambda (exn)
                  (send-to-drscheme-eventspace
                   (lambda ()
                     (handle-exception exn)))
                  (k))]
               [return-handler
                (lambda (read parsed)
                  (send-to-drscheme-eventspace
                   (lambda ()
                     (continue-next-expr read parsed))))])
           (d:interface:set-zodiac-phase 'reader)
           (let* ([new-expr (with-handlers
                                ((exn:read? inner-handler))
                              (reader))])
             (return-handler new-expr
                             (if (z:eof? new-expr)
                                 #f
                                 (begin
                                   (d:interface:set-zodiac-phase 'expander)
                                   (with-handlers
                                       ((exn:syntax? inner-handler))
                                     (z:scheme-expand new-expr 'previous user-vocabulary)))))))))))

         
  (define (continue-next-expr read parsed)
    (let/ec k
      (let ([exn-handler (make-exception-handler k)])
        (if (z:eof? read)
            (construct-final-step)
            (let*-values ([(annotated-list envs) (a:annotate (list read) (list parsed) packaged-envs break)]
                          [(annotated) (car annotated-list)])
              (set! packaged-envs envs)
              (set! current-expr parsed)
              (check-for-repeated-names parsed exn-handler)
              (send-to-user-eventspace
               (lambda ()
                 (let/ec k
                   (current-exception-handler (make-exception-handler k))
                   (user-primitive-eval annotated)
                   (send-to-drscheme-eventspace
                    (lambda ()
                      (add-finished-expr)
                      (read-next-expr)))))))))))
         
         
  (define (check-for-repeated-names expr exn-handler)
    (with-handlers
        ((exn:user? exn-handler))
      (when (z:define-values-form? expr)
        (for-each (lambda (name) 
                    (when (check-global-defined name)
                      (error 'check-for-repeated-names
                             "name is already bound: ~s" name)))
                  (map z:varref-var (z:define-values-form-vars expr))))))
         
  (define (add-finished-expr)
    (let ([reconstructed (r:reconstruct-completed current-expr)])
      (set! finished-exprs (append finished-exprs (list reconstructed)))))
         
  
  
  (define held-expr no-sexp)
  (define held-redex no-sexp)
  (define user-process-held-continuation #f)
  
  (define (suspend-user-computation)
    (let/cc ;; this one absolutely must be a cc, not an ec.
        (lambda (k)
          (set! user-process-held-continuation k)
          (terminate-user-thread-computation 'ignored))))  ;; this doesn't return
  
  (define (restart-user-computation)
    (send-to-user-eventspace
     (lambda ()
       (user-process-held-continuation 'also-ignored))))

  (define (break mark-list break-kind returned-value-list)
    (let ([reconstruct-helper
           (lambda (finish-thunk)
             (send-to-drscheme-eventspace
              (lambda ()
                (let* ([reconstruct-pair
                        (r:reconstruct-current current-expr 
                                               mark-list
                                               break-kind
                                               returned-value-list)]
                       [reconstructed (car reconstruct-pair)]
                       [redex (cadr reconstruct-pair)])
                  (finish-thunk reconstructed redex)))))])
      (case break-kind
        [(normal)
         (when (not (r:skip-redex-step? mark-list))
           (reconstruct-helper 
            (lambda (reconstructed redex)
              (set! held-expr reconstructed)
              (set! held-redex redex)
              (restart-user-computation)))
           (suspend-user-computation))]
        [(result-break)
         (when (if (not (null? returned-value-list))
                   (not (r:skip-redex-step? mark-list))
                   (and (not (eq? held-expr no-sexp))
                        (not (r:skip-result-step? mark-list))))
           (reconstruct-helper 
            (lambda (reconstructed redex)
              (let ([step-text (make-object stepper-text% 
                                            held-expr
                                            held-redex
                                            reconstructed
                                            redex
                                            break-kind
                                            #f)])
                (set! held-expr no-sexp)
                (set! held-redex no-sexp)
                (set! history (append history (list step-text)))
                (update-view view-currently-updating))))
           (suspend-user-computation))])))

         (define (construct-final-step)
           (let ([step-text (make-object stepper-text% no-sexp no-sexp no-sexp no-sexp #f #f)])
             (set! history (append history (list step-text)))
             (set! final-view view-currently-updating)
             (update-view view-currently-updating)))

         (define (handle-exception exn)
           (let ([step-text (if held-expr
                                (make-object stepper-text% held-expr held-redex no-sexp no-sexp #f (exn-message exn))
                                (make-object stepper-text% no-sexp no-sexp no-sexp no-sexp #f (exn-message exn)))])
             (set! history (append history (list step-text)))
             (set! final-view view-currently-updating)
             (update-view view-currently-updating)))
         
         
         (define (make-exception-handler k)
           (lambda (exn)
             (parameterize ([current-eventspace drscheme-eventspace])
               (queue-callback
                (lambda ()
                  (handle-exception exn))))
             (k)))
             
             
         (define (update-view/next-step new-view)
           (set! view-currently-updating new-view)
           (semaphore-post stepper-semaphore)))
      
      (send drscheme-frame stepper-frame s-frame)
      (setup-print-convert settings)
      (set! finished-exprs null)
      (set! view-currently-updating 0)
      (begin-next-expr)
      (send button-panel stretchable-width #f)
      (send button-panel stretchable-height #f)
      (send canvas stretchable-height #t)
      (send canvas min-width 500)
      (send canvas min-height 500)
      (send previous-button enable #f)
      (send home-button enable #f)
      (send next-button enable #f)
      (send (send s-frame edit-menu:get-undo-item) enable #f)
      (send (send s-frame edit-menu:get-redo-item) enable #f)
      (send s-frame show #t)))
  
  (lambda (frame)
    (let ([settings (f:preferences:get 'drscheme:settings)])
      (if (not (string=? (d:basis:setting-name settings) beginner-level-name))
          (message-box "Stepper" 
                       (format (string-append "Language level is set to \"~a\".~n"
                                              "The Foot only works for the \"~a\" language level.~n")
                               (d:basis:setting-name settings)
                               beginner-level-name)
                       #f 
                       '(ok))
          (stepper-go frame settings)))))
