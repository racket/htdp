;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname check-satisfied7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; ASSUME: THIS FILE is IN ISL+

;; all these tests just work out fine
;; ----------------------------------

(define (id x) x)

(check-satisfied 4 (id even?))

(define (okay? x) (member? x (list 0 1 2 3 4 5 6 7 8 9)))
(check-satisfied (random 10) (id okay?))

;; runs fine, even if the predicate is defined below the check-satisfied test

(check-satisfied (random 10) (id between-0-and-10?))
(define (between-0-and-10? x) (member? x (list 0 1 2 3 4 5 6 7 8 9)))


(define (ho x)
  (lambda (y)
    (equal? x y)))

(check-satisfied 10 (ho 10))