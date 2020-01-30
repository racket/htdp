#lang racket/base
(require images/compile-time
         (for-syntax images/icons/control
                     images/icons/style
                     racket/base))
(provide step-img)
;; the bitmap to use in a horizontal or vertical toolbar:
(define step-img (compiled-bitmap (step-icon #:color run-icon-color #:height (toolbar-icon-height))))
