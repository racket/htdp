#lang racket/gui

(require rackunit)

(provide
  ;; syntax
  ;; (testing e ...) creates a top-level test in its own eventspace that
  ;; shuts down after the tests are run 
  testing
  (all-from-out rackunit))

;; -----------------------------------------------------------------------------

(define-syntax-rule
  (testing e ...)
  (begin ;; module+ test
    (do-testing (lambda () e ...))))

(define (do-testing thunk)
  (parameterize ((current-eventspace (make-eventspace)))
    (define ch (make-channel))
    (queue-callback
     (lambda ()
       (with-handlers ([exn? (lambda (e) (channel-put ch e))])
         (channel-put
          ch
          (call-with-values thunk list)))))
    (define r (channel-get ch))
    (if (list? r)
        (apply values r)
        (raise r))))
