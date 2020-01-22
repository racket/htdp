#lang htdp/asl
(require test-engine/test-engine)
(require rackunit)

(define (assert-signature-violation got)
  (begin
    (let ((violations
           (test-object-signature-violations
            (current-test-object))))
      (begin
        (check-equal? (length violations) 1)
        (let* ((violation (car violations))
               (message (signature-violation-message violation)))
          (begin
            (check-pred signature-got? message)
            (check-equal? (signature-got-value message)
                          got)))))
    (initialize-test-object!)))

(: a Integer)
(define a "foo")

(assert-signature-violation "foo")
