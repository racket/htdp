#lang scheme/base
(require scheme/class string-constants/string-constant
         (prefix-in x: "private/step-img.rkt")
         (for-syntax racket/base))
(provide stepper-drracket-button stepper-button-callback)

; hack to make sure the key gets generated once at compile time, and
; not each time this module is instantiated
(define-syntax (unique-member-name-key stx)
  (syntax-case stx ()
    ((_)
     #`(member-name-key #,(gensym 'stepper-button-callback)))))

(define-member-name stepper-button-callback (unique-member-name-key))

; configure is a thunk to configure the runtime settings for printing values
(define (stepper-drracket-button language settings configure)
  (list 
   (string-constant stepper-button-label)
   x:step-img
   (λ (drs-frame)
     (configure)
     (define tab (send drs-frame get-current-tab))
     (parameterize
         ;; make sure output, say from failed check-expects, goes into the REPL
         ((current-output-port (send (send tab get-ints) get-out-port)))
       (send tab stepper-button-callback language settings)))))
