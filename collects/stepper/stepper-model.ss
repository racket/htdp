(require-library "errortrace.ss" "errortrace")


(unit/sig stepper:model^
  (import [i : stepper:model-input^]
          mred^
          [z : zodiac:system^]
          [d : drscheme:export^]
          [p : mzlib:print-convert^]
          [e : stepper:error^]
          [a : stepper:annotate^]
          [r : stepper:reconstruct^]
          stepper:shared^)
  
  (define image? i:image?)
  
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
    
  (define finished-exprs null)

  (define current-expr #f)
  (define packaged-envs a:initial-env-package)
         
  (define user-eventspace (make-eventspace))
         
  ;; user eventspace management
  (define stepper-semaphore (make-semaphore))
  (define stepper-return-val-semaphore (make-semaphore))
  (define stepper-awaken-arg #f)
  (define eval-depth 0)
  
  (define (suspend-user-computation)
    (semaphore-wait stepper-semaphore)
    (cond 
      [(eq? stepper-awaken-arg 'step)
       (void)]
      [(procedure? stepper-awaken-arg)
       (set! eval-depth (+ eval-depth 1))
       (stepper-awaken-arg)
       (set! eval-depth (- eval-depth 1))
       (suspend-user-computation)]))

  (define (continue-user-computation)
    (set! stepper-awaken-arg 'step)
    (semaphore-post stepper-semaphore))
  
  (define (send-to-user-eventspace thunk)
    (set! stepper-awaken-arg thunk)
    (semaphore-post stepper-semaphore))
  
  ;; start user thread going
  (send-to-other-eventspace
   user-eventspace
   suspend-user-computation)

  (define user-primitive-eval #f)
  (define user-vocabulary #f)
         
  (define reader 
    (z:read i:text-stream
            (z:make-location 1 1 0 "stepper-text")))
  
  (printf "preparing to initialize~n")
  (send-to-user-eventspace 
   (lambda ()
     (printf "in the user's eventspace~n")
     (set! user-primitive-eval (current-eval))
     (d:basis:initialize-parameters (make-custodian) i:settings)
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
     (printf "finished initialization~n")
     (semaphore-post stepper-return-val-semaphore)
     (printf "released semaphore~n"))
   (printf "now waiting for semaphore~n")
   (semaphore-wait stepper-return-val-semaphore)
   (printf "semaphore released~n"))
  
  (define print-convert
    (let ([print-convert-result 'not-a-real-value])    
      (lambda (val)
        (send-to-user-eventspace
         (lambda ()
           (set! print-convert-result
                 (p:print-convert val))
           (semaphore-post stepper-return-val-semaphore)))
        (semaphore-wait stepper-return-val-semaphore)
        print-convert-result)))
  
  (define (read-next-expr)
    (send-to-user-eventspace
     (lambda ()
       (let/ec k
         (let ([exception-handler (make-exception-handler k)])
           (d:interface:set-zodiac-phase 'reader)
           (let* ([new-expr (with-handlers
                                ((exn:read? exception-handler))
                              (reader))]
                  [new-parsed (if (z:eof? new-expr)
                                 #f
                                 (begin
                                   (d:interface:set-zodiac-phase 'expander)
                                   (with-handlers
                                       ((exn:syntax? exception-handler))
                                     (z:scheme-expand new-expr 'previous user-vocabulary))))])
             (send-to-drscheme-eventspace
              (lambda ()
                (continue-next-expr new-expr new-parsed)))))))))

         
  (define (continue-next-expr read parsed)
    (let/ec k
      (let ([exn-handler (make-exception-handler k)])
        (if (z:eof? read)
            (stored-return-thunk (make-finished-result finished-exprs))
            (let*-values ([(annotated-list envs) (a:annotate (list read) (list parsed) packaged-envs break)]
                          [(annotated) (car annotated-list)])
              (set! packaged-envs envs)
              (set! current-expr parsed)
              (check-for-repeated-names parsed exn-handler)
              (send-to-user-eventspace
               (lambda ()
                 (let/cc k
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
              (continue-user-computation)))
           (suspend-user-computation))]
        [(result-break)
         (when (if (not (null? returned-value-list))
                   (not (r:skip-redex-step? mark-list))
                   (and (not (eq? held-expr no-sexp))
                        (not (r:skip-result-step? mark-list))))
           (reconstruct-helper 
            (lambda (reconstructed reduct)
              (let ([result (make-before-after-result finished-exprs
                                                      reconstructed
                                                      held-redex
                                                      reduct)])
                (set! held-expr no-sexp)
                (set! held-redex no-sexp)
                (stored-return-thunk result))))
           (suspend-user-computation))])))
  
  (define (handle-exception exn)
    (if held-expr
        (stored-return-thunk (make-before-error-result finished-exprs held-expr held-redex (exn-message exn)))
        (stored-return-thunk (make-error-result finished-exprs (exn-message exn)))))
           
  (define (make-exception-handler k)
    (lambda (exn)
      (send-to-drscheme-eventspace
       (lambda ()
         (handle-exception exn)))
      (k)))

  (define stored-return-thunk 'no-thunk-here)
  
  ; result of invoking stepper-instance : ((stepper-text-args ->) ->)
  (lambda (return-thunk)
    (set! stored-return-thunk return-thunk)
    (read-next-expr)))