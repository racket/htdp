#lang info

(define collection 'multi)
(define deps '("base"
               "htdp-lib"))
(define build-deps '("pict-lib"
                     "redex-lib"
                     "racket-index"
                     "scheme-lib"
                     "srfi-lite-lib"
                     "compatibility-lib"
                     "gui-lib"
                     "racket-test"
                     "rackunit-lib"
                     "profile-lib"
                     "wxme-lib"
                     "pconvert-lib"))
(define update-implies '("htdp-lib"))

(define pkg-desc "tests for \"htdp\"")

(define pkg-authors '(matthias mflatt robby))
