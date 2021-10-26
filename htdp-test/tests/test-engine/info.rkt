#lang info

(define test-responsibles '((all (matthias sperber))))

(define test-omit-paths '("check-satisfied1.rkt"
                          "check-satisfied2.rkt"
                          "check-satisfied3.rkt"
                          "check-satisfied4.rkt"
                          "check-satisfied5.rkt"
                          "check-satisfied8.rkt"
			  "check-satisfied9.rkt"
			  "check-failed-bsl.rkt"
			  "check-failed-isl.rkt"
                          "check-ordered.rkt"))

(define compile-omit-paths '("check-satisfied1.rkt"))
