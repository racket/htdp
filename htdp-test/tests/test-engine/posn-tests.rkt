#lang racket
(require "test-engine-program-test.rkt")
(require test-engine/test-engine)

; Posn tests

(check-success #<<--
#lang htdp/isl+
(: foo Posn)
(define foo (make-posn 1 2))
--
               )

(check-signature-violation #<<--
#lang htdp/isl+
(: foo Posn)
(define foo 5)
--
                           5)

(check-success #<<--
#lang htdp/isl+
(check-property
 (for-all
     ((psn (PosnOf Real Real)))
   (real? (posn-x psn))))
--
               )

(check-failure #<<--
#lang htdp/isl+
(check-property
 (for-all
     ((psn (PosnOf Real Real)))
   (string? (posn-x psn))))
--
              property-fail? )
