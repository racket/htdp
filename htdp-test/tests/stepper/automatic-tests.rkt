#lang racket
(require (only-in test-engine/racket-tests)) ;; only for effect to attach (below)

;; now, the rest:
(require "through-tests.rkt" 
         "test-engine.rkt")

(define lazy-tests 
  '(lazy1 lazy2 lazy3 lazy-multi lazy-app1 lazy-app2 lazy-app3 
    lazy-cons1 lazy-cons2 lazy-list1 lazy-list2 lazy-list3 lazy-list4 lazy-list5
    lazy-caar lazy-cadr lazy-cdar lazy-cddr lazy-caaar lazy-caadr lazy-cadar
    lazy-caddr lazy-cdaar lazy-cdadr lazy-cddar lazy-cdddr lazy-caaaar 
    lazy-caaadr lazy-caadar lazy-caaddr lazy-cadaar lazy-cadadr lazy-caddar
    lazy-cadddr lazy-cdaaar lazy-cdaadr lazy-cdadar lazy-cdaddr lazy-cddaar
    lazy-cddadr lazy-cdddar lazy-cddddr lazy-second lazy-third lazy-fourth
    lazy-fifth lazy-sixth lazy-seventh lazy-eighth
    lazy-if1 lazy-if2 lazy-take-0 lazy-take lazy-take-impl
    lazy-unknown1 lazy-unknown2 lazy-inf-list1 lazy-cond1 lazy-cond2 lazy-cond3
    lazy-eq? lazy-eqv? lazy-equal? lazy-list?1 lazy-list?2 lazy-list?3
    lazy-length lazy-list-ref lazy-list-tail lazy-append lazy-reverse lazy-empty? 
    lazy-assoc lazy-assq lazy-assv lazy-cons? lazy-remove lazy-remq lazy-remv
    lazy-member lazy-memq lazy-memv lazy-filter1 lazy-filter2 lazy-fold
    lazy-cyclic1 lazy-fn-app))

;; these are tests of forms that don't appear below
;; "advanced". These tests are essentially test-driven-development
;; for a stepper for advanced. Some of them don't apply even to advanced....
(define advanced-language-feature-tests
  '(implicit-let set! local-set! 
    call/cc))

;; these are tests of known bugs. These should be fixed. 
(define known-bug-tests
  '(local-struct/ilam
    local-struct/i
    begin-let-bug
    qq-splice))

;; this test anticipates the implementation of the stepper
;; for check-random, which is not yet implemented
(define new-feature-tests
  '(check-random))

(parameterize ([display-only-errors #t]
               ;; display-only-errors is insufficient, because the evals
               ;; actually cause output.  So we just eat stdout.
               [current-output-port (open-output-string)])
  (if (run-all-tests-except
       (append advanced-language-feature-tests
               known-bug-tests
               new-feature-tests))
      (exit 0)
      (exit 1)))
