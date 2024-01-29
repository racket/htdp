;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname check-ordered) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; #lang htdp/bsl

;; it is f that is called before defined not g 
;; SOLVED #87 by Mike S.'s fix of other problem
;; (but the rendering of the output sucks. It seems to lack a newline.)

(check-expect (g 30) "goodbye")
(f 40)
(define (f x) "hello")
(define (g y) "goodbye")
