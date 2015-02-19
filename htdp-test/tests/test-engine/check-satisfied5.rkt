;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname check-satisfied5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; ASSUME: THIS FILE is IN ISL 

(define (id x) x)
(check-satisfied 3 (id equal?))
;; just error, w/o showing the report window but show string below 
"check-satisfied: expects function of one argument in second position. Given equal?"
