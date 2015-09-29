#lang racket/base

;; this parameter is used by the global port
;; print handler in the #lang htdp/* languages
;; in order to have a default that works better,
;; but to still give the opportunity to change it
;; in situations when that's needed.
(define htdp-print-columns (make-parameter 'infinity))
(provide htdp-print-columns)