#lang racket
(require test-engine/racket-tests
         rackunit)

;; this test case grabs all the output
;; just to check that the right kind of
;; error is printed out (the bug this
;; test case was in response to was
;; a bad error message from the check-satsified
;; implementation)
(check-regexp-match
 #rx"check-satisfied encountered an error instead of the expected kind of value"
 (let ([sp (open-output-string)])
     (string-append
      (with-handlers ([exn:fail? exn-message])
        (parameterize ([current-output-port sp])
          (define (f x) x)
          (define (q? x) x)
          (check-satisfied (f 1 2) q?)
          (test)
          "no failure"))
      (get-output-string sp))))

(check-regexp-match
 #rx"no failure"
 (let ([sp (open-output-string)])
     (string-append
      (with-handlers ([exn:fail? exn-message])
        (parameterize ([current-output-port sp])
          (define (f x) x)
          (define (q? x) x)
          (check-satisfied (f 1 2) q?)
          (test)
          "no failure"))
      (get-output-string sp))))

