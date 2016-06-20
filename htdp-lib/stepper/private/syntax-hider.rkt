#lang racket

;; this file exists entirely to hide syntax objects from Typed
;; Racket. Hopefully we can remove this wrapper once there are
;; chaperones for Syntax objects.


(provide (struct-out sstx))

;; hides a syntax object from TR:
(struct sstx (s) #:transparent)