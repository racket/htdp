; Utiltities for creating languages like syntax in a stepper-compatible way
#lang racket/base

(provide (for-syntax check-expect-maker) ; helper syntax for creating the above
         execute-test ; helper for using check-expect-maker
         test ; run tests and display results
         test-execute ; boolean parameter, says if the tests run
         test-silence ; boolean parameter, says if the test results are printed
         report-signature-violation!)

;; check-expect-maker : syntax? syntax? (listof syntax?) symbol? -> syntax?
;; the common part of all three test forms

; NB: typed/test-engine/type-env-ext and stepper/private/macro-unwind.rkt
;     KNOW THE STRUCTURE OF THE GENERATED SYNTAX.
; TREAD CAREFULLY.

(require (for-syntax racket/base)
         (for-syntax stepper/private/syntax-property)
         test-engine/test-engine
         (only-in deinprogramm/signature/signature
                  exn:fail:contract:signature?
                  exn:fail:contract:signature-obj exn:fail:contract:signature-signature
                  exn:fail:contract:signature-blame)
         test-engine/srcloc
         test-engine/test-markup
         (only-in syntax/macro-testing convert-compile-time-error))

(define-for-syntax (check-expect-maker stx checker-proc-stx test-expr embedded-stxes hint-tag)
  (define bogus-name
    (stepper-syntax-property #`#,(gensym 'test) 'stepper-hide-completed #t))
  (define src-info
    (with-stepper-syntax-properties (['stepper-skip-completely #t])
      #`(srcloc #,@(list #`(quote #,(syntax-source stx))
                         (syntax-line stx)
                         (syntax-column stx)
                         (syntax-position stx)
                         (syntax-span stx)))))
  (define test-expr-checked-for-syntax-error #`(convert-compile-time-error #,test-expr))
  (if (eq? 'module (syntax-local-context))
      #`(define #,bogus-name
          #,(stepper-syntax-property
             #`(add-check-expect-test!
                (lambda ()
                  #,(with-stepper-syntax-properties
                      (['stepper-hint hint-tag]
                       ['stepper-hide-reduction #t]
                       ['stepper-use-val-as-final #t])
                      (quasisyntax/loc stx
                        (#,checker-proc-stx
                         (lambda () #,test-expr-checked-for-syntax-error)
                         #,@embedded-stxes
                         #,src-info)))))
             'stepper-skipto
             (append skipto/cdr
                     skipto/second ;; outer lambda
                     '(syntax-e cdr cdr syntax-e car) ;; inner lambda
                     )))
      #`(add-check-expect-test!
         (lambda ()
           #,(with-stepper-syntax-properties
               (['stepper-hint hint-tag]
                ['stepper-hide-reduction #t]
                ['stepper-use-val-as-final #t])
               (quasisyntax/loc stx
                 (#,checker-proc-stx
                  (lambda () #,test-expr-checked-for-syntax-error)
                  #,@embedded-stxes
                  #,src-info)))))))

; this wrapper is necessary because add-test! has a contract which confuses the stepper
(define (add-check-expect-test! thunk)
  (add-test! thunk))

(define (execute-test src thunk exn:fail->reason)
  (let-values (((test-result exn)
                 (with-handlers ([exn:fail:contract:signature?
                                  (lambda (e)
                                    (values
                                     (violated-signature src
                                                         (exn:fail:contract:signature-obj e)
                                                         (exn:fail:contract:signature-signature e)
                                                         (exn:fail:contract:signature-blame e))
                                     e))]
                                 [exn:fail?
                                  (lambda (e)
                                    (values (exn:fail->reason e) e))])
                   (values (thunk) #f))))
    (if (fail-reason? test-result)
        (begin
          (add-failed-check! (failed-check test-result (and (exn? exn) (exn-srcloc exn))))
          #f)
        #t)))


(define (report-signature-violation! obj signature message blame)
  (let* ([srcloc (continuation-marks-srcloc (current-continuation-marks))]
         [message
          (or message
              (signature-got obj))])
    (add-signature-violation! (signature-violation obj signature message srcloc blame))))

; typed/test-engine/typen-env-ext knows what this expands into
(define-syntax (test stx) 
  (syntax-case stx ()
    [(_)
     (syntax-property
      #'(test*)
      'test-call #t)]))

(define test-execute (make-parameter #t))
(define test-silence (make-parameter #f))

(define (test*)
  (when (test-execute)
    (run-tests!))
  (unless (test-silence)
    (display-test-results! (test-object->markup (current-test-object) (not (test-execute)))))
  ;; make sure we return void - anything else might get printed in the REPL
  (void))



