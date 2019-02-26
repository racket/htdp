#lang racket

(require rackunit)

;; this test case grabs all the output
;; just to check that the right kind of
;; error is printed out (the bug this
;; test case was in response to was
;; a bad error message from the check-satsified
;; implementation)

(define (tester e)
  (let ([sp (open-output-string)])
     (string-append
      (with-handlers ([exn:fail? exn-message])
        (parameterize ([current-output-port sp])
          (eval `(module a racket (require test-engine/racket-tests) ,e (test))
                (make-base-namespace))
          "no failure"))
      (get-output-string sp))))


(check-regexp-match
 #rx"check-error: expects at least 1 argument, but found none"
 (tester '(check-error)))

(check-regexp-match
 #rx"check-error: expects only 2 arguments, but found 3"
 (tester '(check-error (error "hello") 2 3)))

