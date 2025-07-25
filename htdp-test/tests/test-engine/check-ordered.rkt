;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname check-ordered) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; #lang htdp/bsl

;; BUT THE RENDERING OF THE OUTPUT SUCKS. IT SEEMS TO LACK A NEWLINE.

(check-expect (g 30) "goodbye")
(f 40)
(define (f x) "hello")
(define (g y) "goodbye")
