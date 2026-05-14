#lang racket
(provide check-success check-failure check-signature-violation)
(require test-engine/test-engine)
(require rackunit)
(require (only-in deinprogramm/signature/signature signature-violation-proc)
         (only-in test-engine/syntax report-signature-violation!))

(define (run-program-tests prg)
  (initialize-test-object!)
  ;; probably want to consistently use something in place of all of
  ;; the "tmp"s below, changing it for each test case.
  
  (define rmp (make-resolved-module-path 'tmp))
  
  (define reader-stx
    (parameterize ([read-accept-reader #t])
      (read-syntax "tmp.rkt" (open-input-string prg))))
  
  (define ns (make-base-namespace))
  (let ([this-namespace (current-namespace)])
    (parameterize ([current-namespace ns])
      (namespace-attach-module this-namespace
                               'test-engine/test-engine)))

  (define expanded-stx
    (parameterize ([current-namespace ns])
      (expand reader-stx)))

  (define output-string-port (open-output-string "repl"))

  (parameterize ([current-namespace ns]
                 [current-module-declare-name rmp]
                 [current-module-declare-source (path->complete-path "tmp.rkt")]
                 [current-output-port output-string-port]
                 ;; need to bind this here, because the binding is lost by the time we run the test submodule
                 [signature-violation-proc
                  (lambda (obj signature message blame-srcloc)
                    (report-signature-violation! obj signature message blame-srcloc))])
    (eval expanded-stx)
    (dynamic-require `(submod 'tmp test) #f))

  ;; at this point, we've done the equivalent of `raco test ....`
  ;; where ... is a file containing `prg`

  (values (get-output-string output-string-port)
          (current-test-object)))

(define (check-success program)
  (let-values (((output test-object) (run-program-tests program)))
    (let ((failed-checks (test-object-failed-checks test-object)))
      (check-pred null? failed-checks))))

(define (assert-failed-check failed-check reason? . selector+value-list)
  (check-pred failed-check? failed-check)
  (let ((reason (failed-check-reason failed-check)))
    (check-pred reason? reason)
    (let loop ((selector+value-list selector+value-list))
      (if (null? selector+value-list)
          #t
          (let ((selector (car selector+value-list))
                (expected-or-pred (cadr selector+value-list)))
            (if (procedure? expected-or-pred)
                (check-pred expected-or-pred (selector reason))
                (check-equal? (selector reason)
                              expected-or-pred))
            (loop (cddr selector+value-list)))))))

(define (check-failure program reason? . selector+value-list)
  (let-values (((output test-object) (run-program-tests program)))
    (let ((failed-checks (test-object-failed-checks test-object)))
      (check-equal? (length failed-checks) 1)
      (when (null? failed-checks)
        (define names
          (for/fold ([l (object-name reason?)])
                    ([f selector+value-list] [i (in-naturals)] #:when (even? i))
            (~a l ", " (object-name f))))
        (error 'check-failure "expected failed check, none failed (~a)" names))
      (apply assert-failed-check (car failed-checks) reason? selector+value-list))
    (initialize-test-object!)))

(define (check-signature-violation program obj)
  (let-values (((output test-object) (run-program-tests program)))
    (let ((signature-violations (test-object-signature-violations test-object)))
      (check-equal? (length signature-violations) 1)
      (check-equal? obj (signature-violation-obj (car signature-violations))))
    (initialize-test-object!)))

