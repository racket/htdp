; Manage the running and recording of tests and their failures
#lang racket/base
(require racket/contract)

(provide (contract-out
          (struct test-object
            ((tests (listof (-> any)))
             (failed-checks (listof failed-check?))
             (signature-violations (listof signature-violation?)))
            #:omit-constructor)
          (empty-test-object (-> test-object?))
          (current-test-object (-> test-object?))
          (test-object-copy (test-object? . -> . test-object?))
          (test-object=? (test-object? test-object? . -> . boolean?))
          (initialize-test-object! (-> any))
          (add-test! ((-> any) . -> . any))
          (add-failed-check! (failed-check? . -> . any))
          (add-signature-violation! (signature-violation? . -> . any))
          (run-tests! (-> test-object?))

          (struct failed-check ((reason fail-reason?)
                                ; the srcloc? is from a possible exception
                                (srcloc? (or/c #f srcloc?))))
          ; srcloc the source location of the actual check
          (struct fail-reason ((srcloc srcloc?)))

          ; an error happened instead of an expected valuee
          (struct (unexpected-error fail-reason)
            ((srcloc srcloc?)
             (expected any/c)
             (exn exn?)))

          ; wanted to satisfy a predicate, but error happend
          (struct (unsatisfied-error fail-reason)
            ((srcloc srcloc?)
             (name string?)
             (exn exn?)))

          ; some result came out, but not the one we expected
          (struct (unequal fail-reason)
            ((srcloc srcloc?)
             (actual any/c)
             (expected any/c)))

          ; some result came out, but not within the precision expected
          (struct (not-within fail-reason)
            ((srcloc srcloc?)
             (actual any/c)
             (expected any/c)
             (range real?)))

          ; we expected an error, but a different one occurred
          (struct (incorrect-error fail-reason)
            ((srcloc srcloc?)
             (expected string?)
             (exn exn?)))

          ; we expected an error, but a value came out instead
          (struct (expected-error fail-reason)
            ((srcloc srcloc?)
             (message (or/c #f string?))
             (value any/c)))

          ; result was not member of the expected set
          (struct (not-mem fail-reason)
            ((srcloc srcloc?)
             (actual any/c)
             (set (listof any/c))))

          ; result was not member of the expected range
          (struct (not-range fail-reason)
            ((srcloc srcloc?)
             (actual real?)
             (min real?)
             (max real?)))
          
          ; wanted to satisfy a predicate, but no satisfaction
          (struct (satisfied-failed fail-reason)
            ((srcloc srcloc?)
             (actual any/c)
             (name string?)))

          ; unhandled violated signature
          (struct (violated-signature fail-reason)
            ((srcloc srcloc?)
             (obj any/c)
             (signature signature?)
             (blame (or/c #f syntax?))))

          ; the value we got that violated the signature.
          (struct signature-got
            ((value any/c)))

          (struct signature-violation
            ((obj any/c)
             (signature signature?)
             (message (or/c string? signature-got?))
             (srcloc (or/c #f srcloc?))
             (blame (or/c #f syntax?))))

          (struct (property-fail fail-reason)
            ((srcloc srcloc?)
             (result check-result?)))

          (struct (property-error fail-reason)
            ((srcloc srcloc?)
             (exn exn?)))))

(require racket/class
         (only-in deinprogramm/signature/signature signature?)
         (only-in deinprogramm/quickcheck/quickcheck check-result?))

;; Terminology:
;; - a test is a piece of code run for testing
;; - a check is a single assertion within that code

(struct test-object
	(tests ; reverse list of thunks
	 failed-checks ; reverse list of failed-check structs
	 signature-violations ; reverse list of signature-violation structs
	 )
	#:mutable #:transparent)

(define (empty-test-object)
  (test-object '() '() '()))

(define *test-object* (empty-test-object))

(define (initialize-test-object)
  (set-test-object-tests! *test-object* '())
  (set-test-object-failed-checks! *test-object* '())
  (set-test-object-signature-violations! *test-object* '())
  *test-object*)

(define (initialize-test-object!)
  (initialize-test-object)
  (void))

(define (current-test-object)
  *test-object*)

(define (test-object-copy a-test-object)
  (struct-copy test-object a-test-object))

(define (test-object=? test-object-1 test-object-2)
  (equal? test-object-1 test-object-2))

(define (add-test! thunk)
  (let ((test-object (current-test-object)))
    (set-test-object-tests! test-object
			    (cons thunk (test-object-tests test-object)))))

(define (run-tests!)
  (let ((test-object (current-test-object)))
    ; in case we're re-running
    (set-test-object-failed-checks! test-object '())
    ;; signature violations come before running tests, and there's no
    ;; way to easily re-run them, so so don't reset them here
    (for-each (lambda (thunk)
                (thunk))
              (reverse (test-object-tests test-object)))
    test-object))

(define (add-failed-check! failed-check)
  (let ((test-object (current-test-object)))
    (set-test-object-failed-checks! test-object
				    (cons failed-check (test-object-failed-checks test-object)))))

(define (add-signature-violation! signature-violation)
  (let ((test-object (current-test-object)))
    (set-test-object-signature-violations! test-object
					   (cons signature-violation
						 (test-object-signature-violations test-object)))))

(struct failed-check (reason srcloc?)
  #:transparent)

(struct fail-reason (srcloc)
  #:transparent)

(struct unexpected-error fail-reason (expected exn)
  #:transparent)

(struct unsatisfied-error fail-reason (name exn)
  #:transparent)

(struct unequal fail-reason (actual expected)
  #:transparent)

(struct not-within fail-reason (actual expected range)
  #:transparent)

(struct incorrect-error fail-reason (expected exn)
  #:transparent)

(struct expected-error fail-reason (message value)
  #:transparent)

(struct not-mem fail-reason (actual set)
  #:transparent)

(struct not-range fail-reason (actual min max)
  #:transparent)

(struct satisfied-failed fail-reason (actual name)
  #:transparent)

; Usually, a language should install a handler for signature violations
; that directly calls add-signature-violation!
; The default handler raises an exception - when that happens
; during a test, this test failure is registered:
(struct violated-signature fail-reason (obj signature blame)
  #:transparent)

(struct signature-got (value)
  #:transparent)

(struct signature-violation (obj signature message srcloc blame)
  #:transparent)

(struct property-fail fail-reason (result)
  #:transparent)
(struct property-error fail-reason (exn)
  #:transparent)

