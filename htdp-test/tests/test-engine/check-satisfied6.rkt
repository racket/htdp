;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname check-satisfied6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; ASSUME: THIS FILE is IN BSL 

;; all these tests just work out fine
;; ----------------------------------

(check-satisfied 4 even?)

(define (okay? x) (member? x (list 0 1 2 3 4 5 6 7 8 9)))
(check-satisfied (random 10) okay?)

;; runs fine, even if the predicate is defined below the check-satisfied test

(check-satisfied (random 10) between-0-and-10?)
(define (between-0-and-10? x) (member? x (list 0 1 2 3 4 5 6 7 8 9)))


