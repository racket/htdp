
;; Basic checks for the intermediate language. See also
;;  beginner.ss

(load-relative "loadtest.ss")

;; Don't need these:
(define no-extra-if-tests? #t)

;; After we require beginner, thunks no longer work:
(define (do-report-errs ignored)
  (report-errs))

(require (lib "intermediate.ss" "lang"))

(load-relative "beg-adv.ss")
(load-relative "beg-intm.ss")
(load-relative "intm-adv.ss")

(syntax-test #'(let name () 10))

(do-report-errs #t)
