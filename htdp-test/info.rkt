#lang info

(define collection 'multi)
(define deps '("base"
               "htdp-lib"))
(define build-deps '("lazy"
                     "deinprogramm"
                     "pict-lib"
                     "simple-tree-text-markup-lib"
                     "redex-lib"
                     "racket-index"
                     "scheme-lib"
                     "compatibility-lib"
                     "gui-lib"
                     "racket-test"
                     "rackunit-lib"
                     "profile-lib"
                     "wxme-lib"
                     "pconvert-lib"
                     "at-exp-lib"
                     "drracket-tool-lib"))
(define update-implies '("htdp-lib"))

(define pkg-desc "tests for \"htdp\"")

(define pkg-authors '(matthias mflatt robby "sperber@deinprogramm.de"))

(define license
  '(Apache-2.0 OR MIT))
