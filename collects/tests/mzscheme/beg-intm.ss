
;; For every test here, make sure the opposite test is in advanced.ss

(syntax-test #'(1 2 3))
(syntax-test #'("hello" 1 2))

(syntax-test #'(define (x) 10))
(syntax-test #'(define x (lambda () 10)))
