
;; Basic checks for the intermediate language. See also
;;  beginner.ss

(load-relative "loadtest.ss")

;; Don't need these:
(define no-extra-if-tests? #t)

;; After we require beginner, thunks no longer work:
(define (do-report-errs ignored)
  (report-errs))

;; Check export names:
(require (lib "docprovide.ss" "syntax"))
(let ([docs (lookup-documentation '(lib "intermediate.ss" "lang") 'procedures)])
  (for-each
   (lambda (row)
     (for-each
      (lambda (doc)
	(let ([v (dynamic-require '(lib "intermediate.ss" "lang") (car doc))])
	  (when (procedure? v)
	    (test (car doc) object-name v))))
      (cdr row)))
   docs))

(require (lib "intermediate.ss" "lang"))

(load-relative "beg-adv.ss")
(load-relative "beg-intm.ss")
(load-relative "intm-adv.ss")

(syntax-test #'(recur empty-f () 10))

(syntax-test #'(let name ([x 12]) 10))

(do-report-errs #t)
