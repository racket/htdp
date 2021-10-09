#lang info

(define collection 'multi)

(define deps
  '("base"
    "scribble-lib"
    "at-exp-lib"
    "draw-lib"
    ["gui-lib" #:version "1.37"]
    "htdp-lib"
    "plai"
    "sandbox-lib"
    "pict-lib"))
(define build-deps '("mzscheme-doc"
                     "scheme-lib"
                     "compatibility-doc"
                     "draw-doc"
                     "drracket"
                     "gui-doc"
                     "pict-doc"
                     "racket-doc"
                     #;"at-exp-lib"
                     #;"rackunit-lib"))
(define update-implies '("htdp-lib"))

(define pkg-desc "documentation part of \"htdp\"")

(define pkg-authors '(matthias mflatt robby "sperber@deinprogramm.de"))

(define license
  '(Apache-2.0 OR MIT))
