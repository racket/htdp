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

(define (stepper-drracket-button language settings)
  (list 
   (string-constant stepper-button-label)
   x:step-img
   (λ (drs-frame) (send (send drs-frame get-current-tab) stepper-button-callback language settings))))
