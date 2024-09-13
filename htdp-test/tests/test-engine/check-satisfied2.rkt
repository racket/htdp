#lang htdp/bsl

;; ASSUME: THIS FILE is IN BSL 

(check-satisfied (random 10) 11)
;; raise run-time error w/o poping up report window but show string below first 

"check-satisfied: expects function of one argument in second position. Given 11"
