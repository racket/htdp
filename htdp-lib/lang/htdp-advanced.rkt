#lang scheme/base

(require "private/teach.rkt"
         "private/teach-module-begin.rkt"
         mzlib/etc
         mzlib/list
         mzlib/pretty
         syntax/docprovide
         scheme/promise
         test-engine/racket-tests
         "posn.rkt")

(require "private/provide-and-scribble.rkt")


;; syntax:
(provide (rename-out
          [advanced-define define]
          [advanced-define-struct define-struct]
          [advanced-define-datatype define-datatype]
          [advanced-lambda lambda]
          [advanced-lambda λ]
          [advanced-app #%app]
          [beginner-top #%top]
          [intermediate-local local]
          [advanced-let let]
          [intermediate-let* let*]
          [intermediate-letrec letrec]
          [advanced-recur recur]
          [beginner-cond cond]
          [beginner-else else]
          [beginner-if if]
          [beginner-and and]
          [beginner-or or]
          [beginner-require require]
          [beginner-dots ..]
          [beginner-dots ...]
          [beginner-dots ....]
          [beginner-dots .....]
          [beginner-dots ......]
          [beginner-true true]
          [beginner-false false]
          [intermediate-quote quote]
          [intermediate-quasiquote quasiquote]
          [intermediate-unquote unquote]
          [intermediate-unquote-splicing unquote-splicing]
          [intermediate-time time]
          [advanced-begin begin]
          [advanced-begin0 begin0]
          [advanced-shared shared]
          [advanced-set! set!]
          [advanced-when when]
          [advanced-unless unless]
          [advanced-case case]
          [advanced-match match]
          [advanced-delay delay]
          [advanced-module-begin #%module-begin])
         check-expect
         check-random
	 check-satisfied
         check-within
         check-error
         check-member-of
         check-range
         #%datum
         #%top-interaction
         empty
         
         signature : -> mixed enum predicate combined
         Number Real Rational Integer Natural Boolean True False String Symbol Char Any
         ConsOf ListOf EmptyList
         Property
         check-property for-all ===> expect expect-within expect-member-of expect-range)

;; procedures:
(provide-and-scribble
 procedures
 (begin)
 (all-from-except intermediateplus: lang/private/intermediate-plus procedures cons list* append random for-each)
 (all-from        advanced:         lang/private/advanced-funs     procedures))
