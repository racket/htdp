
;; For every test here, make sure the opposite test is in intml-adv.ss

(syntax-test #'(1 2 3))
(syntax-test #'("hello" 1 2))


(syntax-test #'(define (f a) (a))) ; no functions as arguments

(syntax-test #'(define x (lambda (y) (lambda (z) z))))
(syntax-test #'(lambda (x) 10))

(syntax-test #'(lambda (f) (f f)))
