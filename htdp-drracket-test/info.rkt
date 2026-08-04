#lang info

#|
This package contains HtDP tests that launch DrRacket and inspect the execution
results. For example, language-test.rkt sets the language to each teaching language
and runs TL expressions and check-expects. Similarly, module-lang-test.rkt directly
verifies the output of test engine in DrRacket.

Without working with DrRacket, it's rather difficult to fully set up everything
and observe the expected behavior, such as image output, the interaction between
printer configuration and test engine, or the right language bindings.
See, e.g., racket/htdp@e6b79368c and racket/htdp@dc985b3cc for the missing setup.

The infrastructure for this package is copied from Robby's drracket-test.
Due to its dependency on DrRacket, this package is separated
from the more self-contained htdp-test package.

There may be some overlap between the tests in this package and
those in drracket-test, but the primary goal of drracket-test is
testing DrRacket while this package aims at testing HtDP.
|#

(define collection 'multi)
(define deps '("base"
               "htdp-lib"))
(define build-deps '("at-exp-lib"
                     "rackunit"
                     "gui-lib"
                     "drracket"))
(define update-implies '("htdp-lib"))

(define pkg-desc "extra tests for \"htdp\" that depend on DrRacket integration")

(define license
  '(Apache-2.0 OR MIT))
