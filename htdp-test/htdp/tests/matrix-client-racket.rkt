#lang racket

;; a Racket (not *SL) client of the matrix library 

(require htdp/matrix)
(require rackunit)

(struct place [x] #:transparent #:property prop:procedure values)

(check-false (equal? (build-matrix 1 1 (λ _ [place 'a])) (build-matrix 1 1 (λ _ [place 'b]))))
(check-equal? (build-matrix 1 1 (λ _ [place 'a])) (build-matrix 1 1 (λ _ [place 'a])))

(define (M i) (build-matrix 1 1 (λ _ [place i])))

(check-false (equal? (M 'a) (M 'b)))
(check-equal? (M 'a) (M 'a))