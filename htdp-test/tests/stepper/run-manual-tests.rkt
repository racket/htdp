#lang racket

(require "through-tests.rkt"
         (prefix-in m: "language-level-model.rkt")
         "test-engine.rkt")

;; this file is for manual checking of individual normally-automated tests.

;; omit from testing
(module* test racket/base)

(printf "ready to run tests.\n")

;; NB: unlike standard linux config-file convention, the values
;; associated with the commented-out parameters are *not* the 
;; default ones, but rather the ones you're likely to want
;; to use instead of the default.
(parameterize (#;[disable-stepper-error-handling #t]
               #;[display-only-errors #t]
               #;[show-all-steps #t]
               #;[ignore-non-lang-tests? #t])
  #;(run-test 'simple-if)
  #;(run-test 'simple-if)
  (run-all-tests)
  
  
  #;(syntax-case
        (first (string->expanded-syntax-list m:intermediate 
                                             "(if true 3 4)"
                                             #;"(letrec ([z 19] [a (lambda (x) (a x))] [b 4]) (+ (a 4) b))"))
      ()
      [(_ _ _ 
          (_ _ (_ _ (_ _ it) _))) #'it])
  )