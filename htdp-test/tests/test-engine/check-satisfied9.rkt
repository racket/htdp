;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname check-satisfied9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; ensure that errors report failure of 'predicateness' properly 

(define (f x) x)
"f [as predicate in check-satisfied]: is expected to return a boolean, but it returned 0"
(check-satisfied 0 f)
