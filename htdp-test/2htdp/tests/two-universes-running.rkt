#lang racket

;; run two universes on the same port on localhost; make sure you gest
;; a sensible error

(require 2htdp/universe)
(require rackunit)

(define (launch tag)
  (universe 0
            [on-tick (λ (x) (displayln `[tag ,x]) (sleep 1) (add1 x)) 1 3]
            [on-msg void]
            [on-new void]))

(define my-custodian (make-custodian))

(parameterize ([current-custodian my-custodian]
               [current-error-port (open-output-string)])
  (check-exn #px"the universe could not be created"
             (λ ()
               (with-output-to-string
                 (λ ()
                   (launch-many-worlds (launch 'one) (launch 'two)))))
             "spawn two universes"))

(custodian-shutdown-all my-custodian)
