#lang racket

;; this file exists entirely to hide syntax objects ... and
;; marks ... from Typed
;; Racket. Hopefully we can remove this wrapper once there are
;; chaperones for Syntax objects.


(provide (struct-out sstx)
         (struct-out smrk))

;; hides a syntax object from TR:
(struct sstx (s) #:transparent)

;; hides a mark from TR
(struct smrk (m) #:transparent)