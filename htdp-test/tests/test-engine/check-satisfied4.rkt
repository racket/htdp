;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname check-satisfied4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; ASSUME: THIS FILE is IN BSL 

(check-satisfied 3 equal?)
;; just error, w/o showing the report window but show string below 
"check-satisfied: expects function of one argument in second position. Given equal?"
