
(syntax-test #'(define (f a) (a))) ; no functions as arguments

(syntax-test #'(define x (lambda (y) (lambda (z) z))))
(syntax-test #'(lambda (x) 10))

(syntax-test #'(lambda (f) (f f)))
