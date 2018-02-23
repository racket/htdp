#lang racket

;; WIP, creating a file to help in the formulation of
;; skipto arguments

;; Holy Moses, this is really hairy.

(require "language-level-model.rkt"
         "test-engine.rkt"
         stepper/private/syntax-property
         stepper/private/shared)

;; this file is for manual checking of individual normally-automated tests.

;; omit from testing
(module* test racket/base)

;; given a syntax object, return the contained syntax object
;; with the "finder" syntax property, or return #f if none
;; can be found.
(define (finder-tagged stx)
  (cond
    [(syntax? stx)
     (cond [(stepper-syntax-property stx 'finder)
            stx]
           [else
            (finder-tagged (syntax-e stx))])]
    [(pair? stx)
     (or (finder-tagged (car stx))
         (finder-tagged (cdr stx)))]
    [(or (null? stx)
         (symbol? stx)
         (number? stx)
         (string? stx)
         (boolean? stx)
         (path? stx))
     #f]
    [else
    (error 'finder-tagged
            "unexpected syntax object: ~e\n"
            stx)]))

;; like finder-tagged, but returns the skipto-path that goes from the
;; given expression to the target expression
;; Syntax -> (U (Listof Symbol) #f)
(define (finder-tagged-path stx)
  (cond
    [(syntax? stx)
     (cond [(stepper-syntax-property stx 'finder)
            '()]
           [else
            (try-path 'syntax-e syntax-e stx)])]
    [(pair? stx)
     (or (try-path 'car car stx)
         (try-path 'cdr cdr stx))]
    [(or (null? stx)
         (symbol? stx)
         (number? stx)
         (string? stx)
         (boolean? stx))
     #f]
    [else
    (error 'finder-tagged
            "unexpected syntax object: ~e\n"
            stx)]))

(define (try-path tag fun stx)
  (path-cons tag (finder-tagged-path (fun stx))))

;; cons the tag onto the path, but not if it's #f
(define (path-cons tag maybe-path)
  (cond [maybe-path (cons tag maybe-path)]
        [else #f]))

(module+ main
  (printf "ready to run tests.\n")


  
  ;; NB: unlike standard linux config-file convention, the values
  ;; associated with the commented-out parameters are *not* the 
  ;; default ones, but rather the ones you're likely to want
  ;; to use instead of the default.
  (parameterize (#;[disable-stepper-error-handling #t]
                 #;[display-only-errors #t]
                 #;[show-all-steps #t]
                 #;[ignore-non-lang-tests? #t])

    ;; needed: models? exp-str
    (define the-ll-model beginner)
    (define exp-str "(check-random (+ 3 4) (+ 2 5)) (+ 4 5)")
    (define extra-files '())

    (define orig-namespace (current-namespace))

    (define (all-expanded expanded-thunk)
      (let loop ()
        (define next (expanded-thunk))
        (cond [(eof-object? next) '()]
              [else (cons next (loop))])))

    (define expanded
      (parameterize ([current-namespace (make-base-namespace)])
        (namespace-attach-module orig-namespace 'mzlib/pconvert-prop)
        (namespace-attach-module orig-namespace 'racket/class)
        (namespace-attach-module orig-namespace 'test-engine/racket-tests)
        (match the-ll-model
          [(struct ll-ll-model (namespace-spec render-settings enable-testing?))
           (namespace-require 'test-engine/racket-tests)
           (match-define (list input-port filename done-thunk)
             (prepare-filesystem exp-str extra-files))
           (define provider-thunk
             (create-provider-thunk namespace-spec enable-testing? input-port))
           (all-expanded (provider-thunk))]
          [(struct ll-hashlang-model (name render-settings enable-testing?))
           (match-define (list input-port filename done-thunk)
             (prepare-filesystem (add-hashlang-line
                                  name
                                  exp-str)
                                 extra-files))
           (define provider-thunk
             (create-hashlang-provider-thunk filename enable-testing? input-port))
           (all-expanded (provider-thunk))])))

    ;; the containing syntax:
    (define outer-stx (finder-tagged expanded))
    (unless outer-stx (error 'annotation-helper
                             "couldn't find outer syntax tagged 'finder"))
    (define inner-stx (finder-tagged (syntax-e outer-stx)))
    (unless inner-stx (error 'annotation-helper
                             "couldn't find outer syntax tagged 'finder"))
    (define path
      (cons 'syntax-e
            (finder-tagged-path (syntax-e outer-stx))))

    ;; let's test it out:
    ;; (not sure why equal? doesn't work on syntax objects...)
    (unless (equal? (syntax->datum (check-path outer-stx path))
                    (syntax->datum inner-stx))
      (error 'path-check-failure))
    
    (list
     path
     outer-stx)
    

    

    )
  )