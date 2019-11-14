#lang htdp/bsl

(check-expect (g 30) "goodbye")
(f 40)
(define (f x) "hello")
(define (g y) "goodbye")
