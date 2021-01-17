#lang racket/base

(require stepper/private/annotate
         "test-engine.rkt"
         "language-level-model.rkt"
         rackunit)

;; does annotation finish without an exception?

;; this is a pathetic "set" of tests, but I just spent time debugging
;; a problem that would have been caught by this test, so I'm adding
;; it anyway. More generally, it might be nice to take every test in
;; the through-tests suite and make sure that it can be annotated without
;; failure before trying to run it.

(define (try-annotating str)
  (define expanded 
    (car (string->expanded-syntax-list intermediate str)))
  ;(printf "expanded: ~s\n" expanded)
  (annotate expanded (lambda (a b c) 'bogus) #f))

(define (try-annotating/hashlang str)
  (define expanded 
    (car (string->expanded-syntax-list intermediate/h str)))
  ;(printf "expanded: ~s\n" expanded)
  (annotate expanded (lambda (a b c) 'bogus) #f))

(module+ test
  (check-not-exn (lambda () (try-annotating "(check-expect 2 2)")))
  
  (check-not-exn
   (lambda ()
     (try-annotating/hashlang "
(/ 3 0)
(+ 9 123)"))))