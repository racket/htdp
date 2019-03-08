#lang racket/load

(load-relative (collection-file-path "loadtest.rktl" "tests/racket"))

(define testing-path (collection-file-path "testing.rktl" "tests/racket"))

(load-in-sandbox "beginner.rktl" #:testing testing-path)
(load-in-sandbox "beginner-abbr.rktl" #:testing testing-path)
(load-in-sandbox "intermediate.rktl" #:testing testing-path)
(load-in-sandbox "intermediate-lambda.rktl" #:testing testing-path)
(load-in-sandbox "advanced.rktl" #:testing testing-path)

(report-errs #t)
