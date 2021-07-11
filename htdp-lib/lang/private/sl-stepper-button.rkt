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
         htdp/bsl/runtime
         stepper/drracket-button)

(define (sl-stepper-drracket-button options)
  (let ([settings (options->sl-runtime-settings options)])
    (stepper-drracket-button (new sl-stepper-language% [settings settings]) settings
                             (lambda () (configure/settings settings)))))

(define-logger stepper)

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
    (define (stepper:show-lambdas-as-lambdas?) (sl-runtime-settings-use-function-output-syntax? settings))

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
          (print-convert val)))

    (public render-value)
    (define (render-value val settings port)
      (parameterize ([print-value-columns +inf.0])
        (print val port)))
    
    (super-instantiate ())))



