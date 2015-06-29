#lang racket

(require (only-in stepper/private/model stepper-model-debug?)
         (prefix-in m: "language-level-model.rkt")
         "test-engine.rkt"
         "test-cases.rkt"
         
         ;; poor man's testing:
         "annotation.rkt"
         
         ;; for xml testing:
         ;; mzlib/class
         ;; (all-except xml/xml-snipclass snip-class)
         ;; (all-except xml/scheme-snipclass snip-class)
         ;; mred
         )

(provide run-test run-tests run-all-tests run-all-tests-except)

;; add all the tests imported from the test cases file(s):
(define list-of-tests
  (for/list ([test-spec (in-list the-test-cases)])
    (match test-spec
      [(list name models string expected-steps extra-files)
       (define models-list
         (cond [(list? models) models]
               [else (list models)]))
       (list name (stepper-test models-list string expected-steps extra-files))])))

;; run a test : (list symbol test-thunk) -> boolean
;; run the named test, return #t if a failure occurred during the test
(define (run-one-test/helper test-pair)
  (run-one-test (car test-pair) (cadr test-pair)))

(define (run-all-tests)
  (andmap/no-shortcut 
   run-one-test/helper
   list-of-tests))

(define (run-all-tests-except nix-list)
  (andmap/no-shortcut 
   run-one-test/helper
   (filter (lambda (pr) (not (member (car pr) nix-list)))
           list-of-tests)))

;; given the name of a test, look it up and run it
(define (run-test name)
  (match (filter (lambda (test) (eq? (first test) name)) list-of-tests)
    [(list) (error 'run-test "test not found: ~.s" name)]
    [(list t) (run-one-test/helper t)]
    [other (error 'run-test "more than one test found with name ~a" name)]))

(define (run-tests names)
  (ormap/no-shortcut run-test names))


;; like an ormap, but without short-cutting
(define (ormap/no-shortcut f args)
  (foldl (lambda (a b) (or a b)) #f (map f args)))

(define (andmap/no-shortcut f args)
  (foldl (lambda (a b) (and a b)) #t (map f args)))


  
  (provide ggg)
  ;; run whatever tests are enabled (intended for interactive use):
  (define (ggg)
    ;; NB: unlike standard linux config-file convention, the values
    ;; associated with the commented-out parameters are *not* the 
    ;; default ones, but rather the ones you're likely to want
    ;; to use instead of the default.
    (parameterize (#;[disable-stepper-error-handling #t]
                   #;[display-only-errors #t]
                   #;[store-steps #f]
                   #;[show-all-steps #t]
                   #;[stepper-model-debug? #t])
      #;(run-tests '(check-expect forward-ref check-within check-within-bad
                                  check-error check-error-bad))
      #;(run-tests '(teachpack-universe))
      #;(run-tests '(top-def-ref top-def-ref))
      (run-all-tests)

      #;(string->expanded-syntax-list m:mz "(if true 3 4)"
                                    #;"(define (a3 x) (if true x x))")
      #;(string->expanded-syntax-list m:intermediate "(letrec ([z 19] [a (lambda (x) (a x))] [b 4]) (+ (a 4) b))")
      
      #;(syntax-case
          (first (string->expanded-syntax-list m:intermediate 
                                               "(if true 3 4)"
                                               #;"(letrec ([z 19] [a (lambda (x) (a x))] [b 4]) (+ (a 4) b))"))
        ()
        [(_ _ _ 
            (_ _ (_ _ (_ _ it) _))) #'it])
      ))


