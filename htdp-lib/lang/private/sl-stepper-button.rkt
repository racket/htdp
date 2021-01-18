; Provide HtDP-specific configuration of the Stepper
#lang racket/base
(provide sl-stepper-drracket-button)
(require racket/class
         racket/pretty
         racket/match
         racket/snip
         (only-in racket/draw bitmap%)
         mzlib/pconvert
         (prefix-in pc: mzlib/pconvert)
         (prefix-in ic: mrlib/image-core)
         mrlib/cache-image-snip
         lang/private/set-result
         lang/stepper-language-interface
         (only-in deinprogramm/signature/signature signature? signature-name)
         stepper/drracket-button)

(define (sl-stepper-drracket-button options)
  (let ([settings (options->settings options)])
    (stepper-drracket-button (new sl-stepper-language% [settings settings]) settings)))

(define-logger stepper)

(define-struct simple-settings (show-sharing
                                insert-newlines
                                true/false/empty-as-ids?
                                abbreviate-cons-as-list
                                use-function-output-syntax?))

(define (present? key options)
  (and (memq key options) #t))

(define (options->settings options)
  (make-simple-settings (present? 'show-sharing options)
                        #t #f ; vestige from old levels
                        (present? 'abbreviate-cons-as-list options)
                        (present? 'use-function-output-syntax options)))
   
(define sl-stepper-language%
  (class* object% (stepper-language<%>)

    (init-field settings)
    
    (public stepper:supported?)
    (define (stepper:supported?) #t)

    (public stepper:enable-let-lifting?)
    (define (stepper:enable-let-lifting?) #f)

    ;; these next three parameters should be overridden by
    ;; the language definition to match the way that the language
    ;; wants these values printed.
    (public stepper:show-lambdas-as-lambdas?)
    (define (stepper:show-lambdas-as-lambdas?) (simple-settings-use-function-output-syntax? settings))

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
                  (simple-module-based-language-convert-value
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
                         ;; apparently some values should be written and some should be printed.
                         ;; Since we formulate a single value to send to the output, this is hard
                         ;; for us.
                         ;; A cheap hack is to print the value and then read it again.
                         ;; Unfortunately, this fails on images. To layer a second hack on
                         ;; the first one, we intercept this failure and just return the
                         ;; value.
                         (with-handlers ([exn:fail:read?
                                          (λ (exn)
                                            (log-stepper-debug
                                             "read fail, print convert returning: ~s\n"
                                             value)
                                            value)])
                           (define result-value
                             (let ([os-port (open-output-string)])
                               (print value os-port)
                               (when (boolean? val)
                                 (log-stepper-debug "string printed by print: ~v\n" (get-output-string os-port)))
                               ;; this 'read' is somewhat scary. I'd like to
                               ;; get rid of this:
                               (read (open-input-string (get-output-string os-port)))))
                           (log-stepper-debug "print-convert returned string that read mapped to: ~s\n" result-value)
                           result-value)])]
                 [(list value)
                  (log-stepper-debug "render-to-sexp: value returned from convert-value: ~v\n" value)
                  value]))))))

    (public render-value)
    (define (render-value val settings port)
      (set-printing-parameters
       settings
       (lambda ()
         (teaching-language-render-value/format val settings port 'infinity))))

    (public set-printing-parameters)
    (define (set-printing-parameters settings thunk)
      (define img-str "#<image>")
      (define (is-image? val)
        (or (is-a? val ic:image%)         ;; 2htdp/image
            (is-a? val cache-image-snip%) ;; htdp/image
            (is-a? val image-snip%)       ;; literal image constant
            (is-a? val bitmap%)))         ;; works in other places, so include it here too
      (define tfe-ids? (simple-settings-true/false/empty-as-ids? settings))
      (parameterize ([pc:booleans-as-true/false tfe-ids?]
                     [pc:add-make-prefix-to-constructor #t]
                     [print-boolean-long-form #t]
                     [pc:abbreviate-cons-as-list (simple-settings-abbreviate-cons-as-list settings)]
                     [pc:current-print-convert-hook
                      (let ([ph (pc:current-print-convert-hook)])
                        (lambda (val basic sub)
                          (cond
                            [(and (not tfe-ids?) (equal? val '())) ''()]
                            [(equal? val set!-result) '(void)]
                            [(signature? val)
                             (or (signature-name val)
                                 '<signature>)]
                            [(bytes? val)
                             (if (< (bytes-length val) 100)
                                 val
                                 (bytes-append (subbytes val 0 99) #"... truncated"))]
                            [else (ph val basic sub)])))]
                     [pretty-print-show-inexactness #t]
                     [pretty-print-exact-as-decimal #t]
                     [pretty-print-print-hook
                      (let ([oh (pretty-print-print-hook)])
                        (λ (val display? port)
                          (if (and (not (port-writes-special? port))
                                   (is-image? val))
                              (begin (display img-str port)
                                     (string-length img-str))
                              (oh val display? port))))]
                     [pretty-print-size-hook
                      (let ([oh (pretty-print-size-hook)])
                        (λ (val display? port)
                          (if (and (not (port-writes-special? port))
                                   (is-image? val))
                              (string-length img-str)
                              (oh val display? port))))]
                     [pc:use-named/undefined-handler
                      (lambda (x)
                        (and (simple-settings-use-function-output-syntax? settings)
                             (procedure? x)
                             (object-name x)))]
                     [pc:named/undefined-handler
                      (lambda (x)
                        (string->symbol
                         (format "function:~a" (object-name x))))])
        (thunk)))
    
    (super-instantiate ())))

(define (teaching-language-render-value/format value settings port width)
  (let*-values ([(converted-value write?)
                 (call-with-values
                  (lambda ()
                    (simple-module-based-language-convert-value value settings))
                  (case-lambda
                    [(converted-value) (values converted-value #t)]
                    [(converted-value write?) (values converted-value write?)]))])
    (let ([pretty-out (if write? pretty-write pretty-print)])
      (cond
        [(simple-settings-insert-newlines settings)
         (if (number? width)
             (parameterize ([pretty-print-columns width])
               (pretty-out converted-value port))
             (pretty-out converted-value port))]
        [else
         (parameterize ([pretty-print-columns 'infinity])
           (pretty-out converted-value port))
         (newline port)]))))

(define (stepper-print-convert-hook exp basic-convert sub-convert)
  (cond
    [(is-a? exp snip%) (send exp copy)]
    [else (basic-convert exp)]))

;; set-print-settings ; settings ( -> TST) -> TST
(define (call-with-print-settings language simple-settings thunk)
  ;; this should succeed for the teaching languges, and fail otherwise.
  ;; if there's a way to directly check this, I should do it. As an approximation,
  ;; the else clause will be guarded by a check for PLTSTEPPERUNSAFE
  (if (method-in-interface? 'set-printing-parameters (object-interface language))
      (send language set-printing-parameters simple-settings thunk)
      ;; should only wind up here for non-teaching-languages:
      (cond [(getenv "PLTSTEPPERUNSAFE") (thunk)]
            [else
             (thunk)
             ;; this error occurs in htdp/bsl etc.
             #;(error
              'stepper-tool
              "language object does not contain set-printing-parameters method")])))

(define (simple-module-based-language-convert-value value settings)
  (parameterize ([constructor-style-printing #t]
                 [show-sharing (simple-settings-show-sharing settings)]
                 [current-print-convert-hook (leave-snips-alone-hook (current-print-convert-hook))])
    (print-convert value)))

(define ((leave-snips-alone-hook sh) expr basic-convert sub-convert)
  (if (or (is-a? expr snip%)
          (is-a? expr bitmap%))
      ; we're missing to-snip here
      expr
      (sh expr basic-convert sub-convert)))


