#lang scheme/base
(require scheme/class string-constants/string-constant
         (prefix-in x: "private/step-img.rkt"))
(provide stepper-button-callback stepper-drracket-button)
(define-local-member-name stepper-button-callback)

(define stepper-drracket-button
  (list 
   (string-constant stepper-button-label)
   x:step-img
   (λ (drs-frame) (send (send drs-frame get-current-tab) stepper-button-callback))))
