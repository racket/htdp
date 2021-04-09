; Generate markup for test results
#lang racket/base

(provide render-value-parameter ; determines how values should be rendered
         display-test-results-parameter ; determines how test-result markup should be rendered
         display-test-results! ; call the above
         get-rewritten-error-message-parameter ; from an exception, extract/translate appropriate message
         get-rewritten-error-message ; call the above
         test-object->markup) ; converts test object to markup

(require (only-in racket/string string-split)
         (only-in racket/list append-map)
         string-constants
         test-engine/test-engine
         simple-tree-text-markup/construct
         simple-tree-text-markup/text
         simple-tree-text-markup/port
         (except-in deinprogramm/signature/signature signature-violation) ; clashes with test-engine
         deinprogramm/quickcheck/quickcheck)

(define render-value-parameter (make-parameter
                                (lambda (v port)
                                  (fprintf port "~V" v))))

(define render-value
  (case-lambda
    ((value)
     (let ((proc (render-value-parameter)))
       (if (procedure-arity-includes? proc 2)
           (let ((port (open-output-string)))
             (proc value port)
             (get-output-string))
           (proc value))))
    ((value port)
     (let ((proc (render-value-parameter)))
       (if (procedure-arity-includes? proc 2)
           (proc value port)
           (display (proc value) port))))))

(define get-rewritten-error-message-parameter
  (make-parameter exn-message))

(define (get-rewritten-error-message exn)
  ((get-rewritten-error-message-parameter) exn))

(define display-test-results-parameter
  (make-parameter
   (lambda (markup)
     (if (port-writes-special? (current-output-port))
         (write-special markup)
         (display-markup markup)))))

(define (display-test-results! markup)
  ((display-test-results-parameter) markup))

(define (test-object->markup test-object [disabled? #f])
  (cond [disabled? (string-constant test-engine-tests-disabled)]
        [(and (null? (test-object-tests test-object))
              (null? (test-object-signature-violations test-object)))
         empty-markup]
        [(and (null? (test-object-failed-checks test-object))
              (null? (test-object-signature-violations test-object)))
         (let ((count (length (test-object-tests test-object))))
           (case count
             [(0) (string-constant test-engine-0-tests-passed)]
             [(1) (string-constant test-engine-1-test-passed)]
             [(2) (string-constant test-engine-both-tests-passed)]
             [else (format (string-constant test-engine-all-n-tests-passed)
                           count)]))]
        [else (test-object-details->markup test-object)]))

(define (test-object-details->markup test-object)
  (let* ([test-count (length (test-object-tests test-object))]
         [failed-checks (reverse (test-object-failed-checks test-object))]
         [failed-check-count (length failed-checks)]
         [signature-violations (reverse (test-object-signature-violations test-object))])
         
    (vertical
     (cond
       [(zero? test-count)
        (string-constant test-engine-must-be-tested)]
       [(= 1 test-count) 
        (string-constant test-engine-ran-1-test)]
       [else
        (format (string-constant test-engine-ran-n-tests) test-count)])

     (if (> test-count 0)
         (vertical
          (cond
            [(and (zero? failed-check-count) (= 1 test-count))
             (string-constant test-engine-1-check-passed)]
            [(zero? failed-check-count) 
             (string-constant test-engine-all-tests-passed)]
            [(= failed-check-count test-count)
             (string-constant test-engine-0-tests-passed)]
            [else (format (string-constant test-engine-m-of-n-tests-failed)
                          failed-check-count test-count)])
          empty-line)
         empty-markup)

     (cond
       ((null? signature-violations) empty-markup)
       ((null? (cdr signature-violations))
        (vertical (string-constant test-engine-1-signature-violation)
                  empty-line))
       (else
        (vertical (format (string-constant test-engine-n-signature-violations)
                          (length signature-violations))
                  empty-line)))
     
     (check-failures->markup failed-checks)
     (signature-violations->markup signature-violations))))

(define (check-failures->markup checks)
  (if (pair? checks)
      (vertical (string-constant test-engine-check-failures)
                (apply vertical
                       (map failed-check->markup checks)))
      empty-markup))

(define (signature-violations->markup violations)
  (if (pair? violations)
      (vertical (string-constant test-engine-signature-violations)
                (apply vertical
                 (map (lambda (violation)
                        (horizontal "        "
                                    (signature-violation->markup violation)))
                      violations)))
      empty-markup))

(define (failed-check->markup failed-check)
  (if (failed-check-srcloc? failed-check)
      (error-link->markup (failed-check-reason failed-check)
                          (failed-check-srcloc? failed-check)
                          (fail-reason-srcloc (failed-check-reason failed-check)))
      (link->markup (failed-check-reason failed-check)
                    (fail-reason-srcloc (failed-check-reason failed-check)))))

(define (link->markup reason srcloc)
  (vertical
   (horizontal "        " (reason->markup reason))
   (srcloc-markup srcloc (format-srcloc srcloc))))

(define (format-srcloc srcloc)
  (let ([line (cond [(srcloc-line srcloc) => number->string]
                    [else 
                     (string-constant test-engine-unknown)])]
        [col
         (cond [(srcloc-column srcloc) => number->string]
               [else (string-constant test-engine-unknown)])]
        [file (srcloc-source srcloc)])
    (if (path? file)
        (let-values (((base name must-be-dir?)
                      (split-path file)))
          (if (path? name)
              (format (string-constant test-engine-in-at-line-column)
                      (path->string name) line col)
              (format (string-constant test-engine-at-line-column)
                      line col)))
        (format (string-constant test-engine-at-line-column)
                line col))))

(define (format->markup format-string . vals)
  (let loop ((chars (string->list format-string))
             (vals vals)
             (rev-markups '())
             (rev-lines '()))
    (cond
      ((null? chars)
       (apply vertical
              (reverse (cons (apply horizontal (reverse rev-markups))
                             (reverse rev-lines))))) ; this will normalize
      ((char=? (car chars) #\~)
       (case (cadr chars)
         ((#\n #\~) (loop (cddr chars) vals
                          '()
                          (cons (apply horizontal (reverse rev-markups))
                                rev-lines)))
         ((#\F #\f)
          (loop (cddr chars)
                (cdr vals)
                (cons (framed-markup (value->markup (car vals))) rev-markups)
                rev-lines))
         (else
          (loop (cddr chars)
                (cdr vals)
                (cons (format (string #\~ (cadr chars)) (car vals)) rev-markups)
                rev-lines))))
      (else
       (let inner-loop ((chars chars)
                        (rev-seen '()))
         (if (or (null? chars)
                 (char=? (car chars) #\~))
             (loop chars vals (cons (list->string (reverse rev-seen)) rev-markups) rev-lines)
             (inner-loop (cdr chars) (cons (car chars) rev-seen))))))))

(define (value->markup value)
  (let-values (((port get-markup)
                (make-markup-output-port/unsafe (lambda (special)
                                                  (image-markup special "#<image>")))))
    (render-value value port)
    (get-markup)))

(define (reason->markup fail)
  (cond
    [(unexpected-error? fail)
     (format->markup (string-constant test-engine-check-encountered-error)
                     (unexpected-error-expected fail)
                     (get-rewritten-error-message (unexpected-error-exn fail)))]
    [(unsatisfied-error? fail)
     (format->markup
      "check-satisfied for ~a encountered an error.\n  :: ~a"
      (unsatisfied-error-name fail)
      (get-rewritten-error-message (unsatisfied-error-exn fail)))]
    [(unequal? fail)
     (format->markup (string-constant test-engine-actual-value-differs-error)
                     (unequal-actual fail)
                     (unequal-expected fail))]
    [(satisfied-failed? fail)
     (format->markup "Actual value ~F does not satisfy ~a."
                     (satisfied-failed-actual fail)
                     (satisfied-failed-name fail))]
    [(not-within? fail)
     (if (string-constant-in-current-language? test-engine-actual-value-not-within-error)
         (format->markup (string-constant test-engine-actual-value-not-within-error)
                         (not-within-actual fail)
                         (not-within-range fail)
                         (not-within-expected fail))
         (format->markup (string-constant test-engine-actual-value-not-within-error/alt-order)
                         (not-within-actual fail)
                         (not-within-expected fail)
                         (not-within-range fail)))]
    [(incorrect-error? fail)
     (format->markup (string-constant test-engine-encountered-error-error)
                     (incorrect-error-expected fail)
                     (get-rewritten-error-message (incorrect-error-exn fail)))]
    [(expected-error? fail)
     (cond
       ((expected-error-message fail)
        => (lambda (message)
             (format->markup (string-constant test-engine-expected-error-error)
                             (expected-error-value fail)
                             message)))
       (else
        (format->markup (string-constant test-engine-expected-an-error-error)
                        (expected-error-value fail))))]
    [(not-mem? fail)
     (horizontal
      (format->markup (string-constant test-engine-not-mem-error)
                      (not-mem-actual fail))
      (apply horizontal
             (map (lambda (a)
                    (format->markup " ~F" a))
                  (not-mem-set fail)))
      ".")]
    [(not-range? fail)
     (format->markup (string-constant test-engine-not-range-error)
                     (not-range-actual fail)
                     (not-range-min fail)
                     (not-range-max fail))]
    [(property-fail? fail)
     (horizontal 
      (string-constant test-engine-property-fail-error)
      (apply horizontal
              (append-map (lambda (arguments)
                            (map (lambda (p)
                                   (if (car p)
                                       (format->markup " ~a = ~F" (car p) (cdr p))
                                       (format->markup "~F" (cdr p))))
                                 arguments))
                          (result-arguments-list (property-fail-result fail)))))]
    [(property-error? fail)
     (format->markup (string-constant test-engine-property-error-error)
                     (get-rewritten-error-message (property-error-exn fail)))]
    ;; should not happen
    [(violated-signature? fail)
     (format->markup "Unhandled signature violation: got ~F, violated signature ~a, to blame: ~a"
                     (violated-signature-obj fail)
                     (signature-name (violated-signature-signature fail))
                     (let ((blame (violated-signature-blame fail)))
                       (if blame
                           (syntax->datum blame)
                           '<unknown>)))]))

(define (error-link->markup reason srcloc check-srcloc)
  (vertical
   (horizontal "        " (reason->markup reason))
   (horizontal
    (srcloc-markup srcloc (format-srcloc check-srcloc))
    (if srcloc
        (horizontal
         " "
         (string-constant test-engine-check-error-cause)
         " "
         (srcloc-markup srcloc (format-srcloc srcloc)))
        empty-markup))))

(define (signature-violation->markup violation)
  (let* ((signature (signature-violation-signature violation))
         (stx (signature-syntax signature))
         (srcloc (signature-violation-srcloc violation))
         (message (signature-violation-message violation)))
    (horizontal
     (cond
       ((string? message) message)
       ((signature-got? message)
        (horizontal (string-constant test-engine-got)
                    " "
                    (framed-markup (value->markup (signature-got-value message)))))
       (else empty-markup))
    
     (if srcloc
         (horizontal " " (srcloc-markup srcloc (format-srcloc srcloc)))
         empty-markup)
     ", "
     (string-constant test-engine-signature)
     " "
     (srcloc-markup (syntax-srcloc stx) (format-srcloc (syntax-srcloc stx)))
     (cond
       ((signature-violation-blame violation)
        => (lambda (blame)
             (horizontal
              "        "
              (string-constant test-engine-to-blame)
              " "
              (srcloc-markup (syntax-srcloc blame) (format-srcloc (syntax-srcloc blame))))))
       (else empty-markup)))))

(define (syntax-srcloc stx)
  (srcloc (syntax-source stx)
          (syntax-line stx) (syntax-column stx)
          (syntax-position stx) (syntax-span stx)))

(module+ test
  (require rackunit
           (only-in simple-tree-text-markup/data markup?)
           (only-in deinprogramm/signature/signature-german make-predicate-signature))

  (parameterize
      ((render-value-parameter
        (lambda (v)
          (format "<~V>" v))))
    (check-equal? (format->markup "abc")
                  "abc")
    (check-equal? (format->markup "foo ~F bar ~v~~bar" 5 #f)
                  (vertical
                   (horizontal "foo "
                               (framed-markup "<5>")
                               " bar "
                               "#f")
                   "bar")))
  
  (parameterize
      ((render-value-parameter
        (lambda (v)
          (format "<~V>" v))))
    (call-with-current-language
     'english
     (lambda ()
       (check-equal? (reason->markup
                      (unexpected-error (srcloc 'source 1 0 10 20) 'expected (exn "not expected" (current-continuation-marks))))
                     (vertical
                      (horizontal
                       "check-expect encountered the following error instead of the expected value, "
                       (framed-markup "<'expected>")
                       ". ")
                      "   :: not expected")))))


  (check-pred
   markup?
   (test-object->markup
    (empty-test-object)))

  (define fail-unexpected-error
    (failed-check
     (unexpected-error (srcloc 'source 1 0 10 20) 'expected (exn "not expected" (current-continuation-marks)))
     (srcloc 'exn 2 1 30 40)))
  (define fail-unsatisfied-error
    (failed-check
     (unsatisfied-error (srcloc 'source 1 0 10 20) "zero?" (exn "not expected" (current-continuation-marks)))
     #f))
  (define fail-unequal
    (failed-check
     (unequal (srcloc 'source 1 0 10 20) 'actual 'expected)
     #f))
  (define fail-not-within
    (failed-check
     (not-within (srcloc 'source 1 0 10 20) #i12 #i14 #i0.5)
     #f))
  (define fail-incorrect-error
    (failed-check
     (incorrect-error (srcloc 'source 1 0 10 20) "expected" (exn "not expected" (current-continuation-marks)))
     #f))
  (define fail-expected-error
    (failed-check
     (expected-error (srcloc 'source 1 0 10 20) "error message" (exn "some other error message" (current-continuation-marks)))
     #f))
  (define fail-an-expected-error
    (failed-check
     (expected-error (srcloc 'source 1 0 10 20) #f (exn "some error message" (current-continuation-marks)))
     #f))
  (define fail-not-mem
    (failed-check
     (not-mem (srcloc 'source 1 0 10 20) 'actual '(expected-1 expected-2))
     #f))
  (define fail-not-range
    (failed-check
     (not-range (srcloc 'source 1 0 10 20) 12 1 10)
     #f))
  (define fail-satisfied-failed
    (failed-check
     (satisfied-failed (srcloc 'source 1 0 10 20) 'actual "string?")
     #f))
  (define fail-property-fail
    (failed-check
     (property-fail (srcloc 'source 1 0 10 20)
                    (make-result #t '()
                                 '(((a . 1) (#f . 2) (c . 3)))))
     #f))
  (define fail-property-error
    (failed-check
     (property-error (srcloc 'source 1 0 10 20) (exn "not expected" (current-continuation-marks)))
     #f))

  (define integer (make-predicate-signature 'integer integer? #'integer-marker))
  (define fail-violated-signature
    (failed-check
     (violated-signature (srcloc 'source 1 0 10 20) 'obj integer #'syntax)
     #f))

  (define signature-violation-1
    (signature-violation 'obj integer (signature-got 'got) (srcloc 'signature 2 3 23 333) #f))
  (define signature-violation-2
    (signature-violation 'obj-2 integer (signature-got 'got-2) (srcloc 'signature 3 4 24 334) #f))
  (define signature-violation-3
    (signature-violation 'obj-3 integer (signature-got 'got-3) (srcloc 'signature 4 5 25 335) #f))

  (define (make-test-object tests failed-checks signature-violations)
    (let ((test-object (empty-test-object)))
      (set-test-object-tests! test-object tests)
      (set-test-object-failed-checks! test-object failed-checks)
      (set-test-object-signature-violations! test-object signature-violations)
      test-object))
  
  ; there are special cases for 1 and 2
  (check-pred
   markup?
   (test-object->markup
    (make-test-object (list void)
                      (list fail-unexpected-error)
                      (list signature-violation-1))))

  (check-pred
   markup?
   (test-object->markup
    (make-test-object (list void void)
                      (list fail-unexpected-error fail-unsatisfied-error)
                      (list signature-violation-1 signature-violation-2))))

  (check-pred
   markup?
   (test-object->markup
    (make-test-object (list void void)
                      (list fail-unexpected-error fail-unsatisfied-error
                            fail-unequal fail-not-within fail-incorrect-error
                            fail-not-mem fail-not-range fail-satisfied-failed
                            fail-property-fail fail-property-error
                            fail-violated-signature)
                      (list signature-violation-1 signature-violation-2 signature-violation-3))))

  
  )


