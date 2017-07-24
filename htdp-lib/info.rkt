#lang info

(define collection 'multi)

(define deps
  '(["base" #:version "6.8.0.2"]
    "compatibility-lib"
    "draw-lib"
    ("drracket-plugin-lib" #:version "1.1")
    "errortrace-lib"
    "html-lib"
    "images-gui-lib"
    "images-lib"
    "net-lib"
    "pconvert-lib"
    "plai-lib"
    "r5rs-lib"
    "sandbox-lib"
    "scheme-lib"
    "scribble-lib"
    "slideshow-lib"
    "snip-lib"
    "srfi-lite-lib"
    ["string-constants-lib" #:version "1.13"]
    "typed-racket-lib"
    "typed-racket-more"
    "web-server-lib"
    "wxme-lib"
    ("gui-lib" #:version "1.7")
    "deinprogramm-signature"
    "pict-lib"))
(define build-deps '("racket-index"
                     "at-exp-lib"
                     "rackunit-lib"))

(define pkg-desc "implementation (no documentation) part of \"htdp\"")

(define pkg-authors '(matthias mflatt robby))

(define version "1.7")
