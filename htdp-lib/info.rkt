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
    ["pconvert-lib" #:version "1.2"]
    "plai-lib"
    "r5rs-lib"
    "sandbox-lib"
    "scheme-lib"
    "scribble-lib"
    ["simple-tree-text-markup-lib" #:version "1.1"]
    "slideshow-lib"
    "snip-lib"
    "srfi-lite-lib"
    ["string-constants-lib" #:version "1.20"]
    "typed-racket-lib"
    "typed-racket-more"
    "web-server-lib"
    "wxme-lib"
    ("gui-lib" #:version "1.80")
    "deinprogramm-signature"
    "pict-lib"))
(define build-deps '("racket-index"
                     "at-exp-lib"
                     ["rackunit-lib" #:version "1.10"]))

(define pkg-desc "implementation (no documentation) part of \"htdp\"")

(define pkg-authors '(matthias mflatt robby "sperber@deinprogramm.de"))

(define version "1.8")

(define license
  '(Apache-2.0 OR MIT))
