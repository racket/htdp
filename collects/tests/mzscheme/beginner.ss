
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

;; Check export names:
(require (lib "docprovide.ss" "syntax"))
(let ([docs (lookup-documentation '(lib "beginner.ss" "lang") 'procedures)])
  (for-each
   (lambda (row)
     (for-each
      (lambda (doc)
	(let ([v (dynamic-require '(lib "beginner.ss" "lang") (car doc))])
	  (when (procedure? v)
	    (test (car doc) object-name v))))
      (cdr row)))
   docs))

(require (lib "beginner.ss" "lang"))

(load-relative "beg-adv.ss")
(load-relative "beg-intm.ss")
(load-relative "beg-bega.ss")

(syntax-test #'quote)
(syntax-test #''1)
(syntax-test #''"hello")
(syntax-test #''(1 2))
(syntax-test #'''a)

(report-errs)
