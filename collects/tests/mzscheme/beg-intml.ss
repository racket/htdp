
;; For every test here, make sure the opposite test is in advanced.ss

(syntax-test #'(define (x) 10))
(syntax-test #'(define x (lambda () 10)))
