
;; For every test here, make sure the opposite test is in advanced.ss

(syntax-test #'(define (xthnk) 10))
(syntax-test #'(define xthnk (lambda () 10)))
