
;; Basic checks for the beginner language. Error messages really
;; should be inspected manually.

;; Limitations of this test suite:
;;  - It doesn't check reader-level parameterization, such as use of quotes
;;  - It doesn't check format of printed results
;;  - It doesn't check the absence of MzScheme forms

;; Don't try to run other tests from the test suite after loading this
;; one into a particular namespace.

(load-relative "loadtest.ss")

;; Don't need these:
(define no-extra-if-tests? #t)

;; After we require beginner, thunks no longer work:
(define (do-report-errs ignored)
  (report-errs))

(require (lib "beginner.ss" "lang"))

(load-relative "beg-adv.ss")
(load-relative "beg-intm.ss")

(syntax-test #'quote)
(syntax-test #''1)
(syntax-test #''"hello")
(syntax-test #''(1 2))
(syntax-test #'''a)

(syntax-test #'(define x (lambda (y) (lambda (z) z))))
(syntax-test #'(lambda (x) 10))

(syntax-test #'(lambda (f) (f f)))

(do-report-errs #t)
