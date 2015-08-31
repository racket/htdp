#lang racket/base

(require stepper/private/model-settings)

(provide (all-defined-out))

;; DEFINING A LANGUAGE FOR THE PURPOSES OF TESTING

;; distant future FIXME: I think we can dump the namespace-spec and the enable-testing
;; when all stepper use is hashlang-ed.

;; ll-model : a representation of the behavior of a language level w.r.t. the stepper
(struct ll-model ())
;; represents tests to be run in the old "set the language level" mode
(struct ll-ll-model ll-model (namespace-spec render-settings enable-testing?))
;; represents tests to be run using a "#lang" declaration
(struct ll-hashlang-model ll-model (name render-settings enable-testing?))

;; the built-in ll-models:
(define mz
  (ll-ll-model 'racket fake-mz-render-settings #f))
#;(define mz/h
  (ll-ll-model 'mzscheme fake-mz-render-settings #f))

(define beginner
  (ll-ll-model `(lib "htdp-beginner.ss" "lang") fake-beginner-render-settings #t))
(define beginner/h
  (ll-hashlang-model "htdp/bsl" fake-beginner-render-settings #t))
(define beginner/both (list beginner beginner/h))

(define beginner-wla
  (ll-ll-model `(lib "htdp-beginner-abbr.ss" "lang") fake-beginner-wla-render-settings #t))
(define beginner-wla/h
  (ll-hashlang-model "htdp/bsl+" fake-beginner-wla-render-settings #t))
(define beginner-wla/both (list beginner-wla beginner-wla/h))

(define intermediate
  (ll-ll-model `(lib "htdp-intermediate.ss" "lang") fake-intermediate-render-settings #t))
(define intermediate/h
  (ll-hashlang-model "htdp/isl" fake-intermediate-render-settings #t))
(define intermediate/both (list intermediate intermediate/h))

(define intermediate-lambda
  (ll-ll-model `(lib "htdp-intermediate-lambda.ss" "lang") fake-intermediate/lambda-render-settings #t))
(define intermediate-lambda/h
  (ll-hashlang-model "htdp/isl+" fake-intermediate/lambda-render-settings #t))
(define intermediate-lambda/both (list intermediate-lambda intermediate-lambda/h))

(define advanced
  (ll-ll-model `(lib "htdp-advanced.ss" "lang") fake-advanced-render-settings #t))
(define advanced/h
  (ll-hashlang-model "htdp/asl" fake-advanced-render-settings #t))
(define advanced/both (list advanced advanced/h))

(define lazy
  (ll-ll-model `(lib "lazy.rkt" "lazy") fake-lazy-render-settings #f))
(define lazy/h
  (ll-hashlang-model "lazy" fake-lazy-render-settings #f))

;; unsure about the render-settings, here: 
(define dmda-a
  (ll-ll-model `(lib "DMdA-beginner.ss" "deinprogramm") fake-beginner-render-settings #t))
;; waiting to hear from Mike Sperber on this....:
#;(define dmda-a/h
  (ll-hashlang-model `(lib "DMdA-beginner.ss" "deinprogramm") fake-beginner-render-settings #t))


;; SUPPORT FOR TESTING A BUNCH OF LANGUAGES AT ONCE:

;; built-in multi-language bundles:
(define upto-int/lam
  (list beginner
        beginner/h
        beginner-wla
        beginner-wla/h
        intermediate
        intermediate/h
        intermediate-lambda
        intermediate-lambda/h
        ))

(define upto-int
  (list beginner
        beginner/h
        beginner-wla
        beginner-wla/h
        intermediate
        intermediate/h))

(define bwla-to-int/lam
  (list beginner-wla
        beginner-wla/h
        intermediate
        intermediate/h
        intermediate-lambda
        intermediate-lambda/h))

(define both-intermediates
  (list intermediate
        intermediate/h
        intermediate-lambda
        intermediate-lambda/h))
