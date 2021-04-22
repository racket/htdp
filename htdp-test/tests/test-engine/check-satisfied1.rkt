;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname check-satisfied1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ASSUME: THIS FILE is IN BSL 

(define (id x) x)

;; *** UNCOMMENT THIS ***
(check-satisfied 9 (id odd?))
;; expected syntax error, can also trigger this with check-syntax
"odd?: expected a function call, but there is no open parenthesis before this function"
