;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname check-satisfied3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; ASSUME: THIS FILE is IN BSL 

;; the following just fail, pop up window and show 

(check-satisfied 4 odd?)
(check-satisfied (+ (random 2) 1) zero?)

