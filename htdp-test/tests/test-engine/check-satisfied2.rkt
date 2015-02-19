;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname check-satisfied2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; ASSUME: THIS FILE is IN BSL 

(check-satisfied (random 10) 11)
;; raise run-time error w/o poping up report window but show string below first 

"check-satisfied: expects function of one argument in second position. Given 11"
