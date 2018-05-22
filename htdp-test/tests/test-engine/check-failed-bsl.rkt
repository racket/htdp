;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname check-failed-bsl) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; MUST BE IN BSL 
;; expect the two errors to be reported as two failed tests
;; f: expects only 1 argument, but found 2

(define (f x) x)
(check-expect (f 1 2) 3)
(check-satisfied (f 1 2) odd?)