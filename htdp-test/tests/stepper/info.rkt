#lang info

(define test-omit-paths '("jump-to-ui-test.rkt"
                          "big-bang-test.rkt"))

(define test-responsibles '((all clements)))
(define test-timeouts '(("run-manual-tests.rkt" 500)
                        ("automatic-tests.rkt" 500)))
