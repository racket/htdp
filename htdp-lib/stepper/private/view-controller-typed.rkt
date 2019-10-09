#lang typed/racket

;; this file contains the parts of the view-controller
;; that can easily be represented using typed racket.

(provide (struct-out Step)
         application-step?
         finished-stepping-step?)

;; the stored representation of a step
(struct Step ([text : Any] ;; should be text%
              [kind : (U 'finished-or-error
                         'user-application
                         'normal)]
              [posns : (Listof Any)]) #:transparent)


;; is this an application step?
(define (application-step? [history-entry : Step]) : Boolean
  (match history-entry
    [(Step _ (or 'user-application 'finished-or-error) posns) #t]
    [else #f]))

;; is this the finished-stepping step?
(define (finished-stepping-step? [history-entry : Step]) : Boolean
  (match (Step-kind history-entry)
    ['finished-or-error #t]
    [else #f]))

