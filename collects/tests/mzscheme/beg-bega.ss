
(syntax-test #'(local [(define x 5)] x))
(syntax-test #'(recur name ([x 18]) x))

(syntax-test #'(define (f a) (a))) ; no functions as arguments

