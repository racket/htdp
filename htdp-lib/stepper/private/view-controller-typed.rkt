#lang typed/racket

;; this file contains the parts of the view-controller
;; that can easily be represented using typed racket.

(provide (struct-out Step))

;; the stored representation of a step
(struct Step ([text : Any] ;; should be text%
              [kind : (U 'finished-or-error
                         ;; why is user-application special-cased?
                         'user-application
                         'normal)]
              [posns : (Listof Any)]) #:transparent)

