
;; For every test here, make sure the opposite test is in advanced.ss

(syntax-test #'(1 2 3))
(syntax-test #'("hello" 1 2))
(syntax-test #'(+))

(syntax-test #'(define (f a) (a))) ; no functions as arguments

(syntax-test #'(define (x) 10))
(syntax-test #'(define (x y) 10 12))
(syntax-test #'(define x (lambda () 10)))

(syntax-test #'(define-struct a ())) ; because the constructor would be useless

