#lang racket

(require "through-tests.rkt")

;; NB: unlike standard linux config-file convention, the values
;; associated with the commented-out parameters are *not* the 
;; default ones, but rather the ones you're likely to want
;; to use instead of the default.
(parameterize (#;[disable-stepper-error-handling #t]
               #;[display-only-errors #t]
               #;[store-steps #f]
               #;[show-all-steps #t])
  (run-test 'lazy-take)
  #;(run-all-tests)
  
  
  #;(syntax-case
        (first (string->expanded-syntax-list m:intermediate 
                                             "(if true 3 4)"
                                             #;"(letrec ([z 19] [a (lambda (x) (a x))] [b 4]) (+ (a 4) b))"))
      ()
      [(_ _ _ 
          (_ _ (_ _ (_ _ it) _))) #'it])
  )