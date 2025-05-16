#lang racket/base

(require (only-in lang/private/teach check-property
                  for-all Integer)
         (only-in lang/htdp-beginner
                  [#%app #%app/bsl]
                  empty)
         (only-in lang/htdp-intermediate-lambda
                  [define define/isl]
                  [#%app #%app/isl]
                  local)
         (only-in htdp/bsl/runtime configure)
         (except-in rackunit check-within)
         racket/port
         (only-in racket/string string-contains?)
         simple-tree-text-markup/text
         test-engine/racket-tests
         test-engine/test-engine)

(require racket/format)

;; To configure error messages
(configure ;; ISL options
 '(abbreviate-cons-as-list
   read-accept-quasiquote
   use-function-output-syntax))

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

(define (check-success)
  (let* ((test-object (run-tests!))
         (failed-checks (test-object-failed-checks test-object)))
    (check-pred null? failed-checks))
  (initialize-test-object!))


(define-syntax-rule (check-failure reason . selector+value-list)
  (check-failure* #'reason reason . selector+value-list))
  
(define (check-failure* src reason? . selector+value-list)
  (let* ((test-object   (run-tests!))
         (failed-checks (test-object-failed-checks test-object)))
    (check-equal? (length failed-checks) 1)
    (when (null? failed-checks)
      (define names
        (for/fold ([l (object-name reason?)])
                  ([f selector+value-list] [i (in-naturals)] #:when (even? i))
          (~a l ", " (object-name f))))
      (error 'check-failure "expected failed check, none failed (~a) (~a)" names src))
    (apply assert-failed-check (car failed-checks) reason? selector+value-list))
  (initialize-test-object!))

(define (count f)
  (cond
    [(zero? f) 1]
    [else (add1 (count (sub1 f)))]))

(check-expect (count 3) 3)
(check-failure unequal?
               unequal-actual 4
               unequal-expected 3)

(check-expect (count 3) 4)
(check-success)

(check-expect 1 2)
(check-failure unequal?
               unequal-actual 1
               unequal-expected 2)

(check-satisfied (cdr (list 3514)) null)
(check-failure unsatisfied-error?
               unsatisfied-error-exn (lambda (e)
                                       (regexp-match? #rx"application: not a procedure"
                                                      (exn-message e)))
               unsatisfied-error/markup-error-markup
               (lambda (m)
                 (regexp-match? #rx"function call: expected a function after the open parenthesis, but received '[(][)]"
                                (with-output-to-string
                                  (lambda () (display-markup m))))))

(define/isl even1
  (local [(define/isl (local-even m k)
            (even? (+ m k)))]
    local-even))

(define/isl (even2 m k)
  (even? (+ m k)))

(check-satisfied 4 even1)
(check-failure unsatisfied-error?
               unsatisfied-error-exn (lambda (e)
                                       (regexp-match? #rx"local-even.*arity mismatch"
                                                      (exn-message e)))
               unsatisfied-error/markup-error-markup
               (lambda (m)
                 (regexp-match? #rx"local-even.*expects 2 arguments.*found only 1"
                                (with-output-to-string
                                  (lambda () (display-markup m))))))

(check-satisfied 4 even2)
(check-failure unsatisfied-error?
               unsatisfied-error-exn
               (lambda (e)
                 (regexp-match?
                  #rx"check-satisfied.*expects function of one argument.*Given even2"
                  (exn-message e)))
               unsatisfied-error/markup-error-markup
               (lambda (m)
                 (regexp-match?
                  #rx"check-satisfied.*expects function of one argument.*Given even2"
                  (with-output-to-string
                    (lambda () (display-markup m))))))

(check-satisfied 0 (lambda (n bad) #t))
(check-exn
 (lambda (e)
   (initialize-test-object!)
   (and (exn:fail:contract? e)
        (regexp-match?
         ;; BUG: error-check in test-engine/racket-tests is NOT using print-convert!
         (pregexp
          (string-append "check-satisfied.*expect.*one argument.*second position.*"
                         "Given.*racket-tests[.]rkt:[[:digit:]]+:[[:digit:]]+"))
         (exn-message e))))
 (lambda ()
   (run-tests!)))

(check-within 1.345 1.3 .05)
(check-success)

(check-within 1.345 1.3 .005)
(check-failure not-within?
               not-within-actual 1.345
               not-within-expected 1.3
               not-within-range 0.005)

(check-within 1.345 1.3 "0.05")
(check-exn
 (lambda (e)
   (initialize-test-object!)
   (and (exn:fail:contract? e)
        (regexp-match? #rx"check-within.*\"0[.]05\" is not inexact"
                       (exn-message e))))
 (lambda ()
   (run-tests!)))

(check-expect (cons 1 (cons 2 (cons 3 '()))) (cons 2 (cons 2 (cons 2 '()))))
(check-failure unequal?
               unequal-actual '(1 2 3)
               unequal-expected '(2 2 2))

(check-expect (cons 1 (cons 2 (cons 3 '()))) '())
(check-failure unequal?
               unequal-actual '(1 2 3)
               unequal-expected '())

(check-expect (cons 1 (cons 2 (cons 3 '()))) (cons 1 (cons 2 (cons 3 '()))))
(check-success)

(check-within (cons 1 (cons 2 (cons 3 '()))) (cons 1.1 (cons 2.1 (cons 3.1 '()))) .2)
(check-success)

(check-within (cons 1 (cons 2 (cons 3 '()))) (cons 1.1 (cons 2.1 (cons 3.1 '()))) .01)
(check-failure not-within?
               not-within-actual '(1 2 3)
               not-within-expected '(1.1 2.1 3.1)
               not-within-range 0.01)

(check-expect 'red 'blue)
(check-failure unequal?
               unequal-actual 'red
               unequal-expected 'blue)

(check-expect 'red 'red)
(check-success)

(check-within 'red 'red .002)
(check-success)

(check-expect 'red "red")
(check-failure unequal?
               unequal-actual 'red
               unequal-expected "red")

(check-expect "red" "red")
(check-success)

(check-expect "red " "red")
(check-failure unequal?
               unequal-actual "red "
               unequal-expected "red")

(check-expect "Hello" "red")
(check-failure unequal?
               unequal-actual "Hello"
               unequal-expected "red")

(check-within "hello" "Hello" .03)
(check-failure not-within?
               not-within-actual "hello"
               not-within-expected "Hello"
               not-within-range 0.03)

(define-struct posn (x y) #:transparent)
(define-struct ball (point rad color) #:transparent)
(define blue-ball (make-ball (make-posn 1 3) 3.4 "blue"))

(check-expect (make-ball 4 5 'blue) (make-ball 4 5 'blue))
(check-success)

(check-expect (make-ball (make-posn 1 2) #e3.5 'blue) (make-ball (make-posn 1 2) #e3.5 'blue))
(check-success)

(check-expect (make-ball 3 (make-posn 1 2) "blue") (make-ball (make-posn 1 2) #e3.3 "blue"))
(check-failure unequal?
               unequal-actual (make-ball 3 (make-posn 1 2) "blue")
               unequal-expected (make-ball (make-posn 1 2) #e3.3 "blue"))

(check-within blue-ball (make-ball (make-posn 1 3) 3.3 "blue") .11)
(check-success)

(check-within blue-ball (make-ball (make-posn 1 3) 3.3 "blue") .01) ;fails
(check-failure not-within?
               not-within-actual blue-ball
               not-within-expected (make-ball (make-posn 1 3) 3.3 "blue")
               not-within-range 0.01)

(check-error (error 'test "hi") "test: hi")
(check-success)

(check-error (error 'test "not hi") "test: hi")
(check-failure incorrect-error?
               incorrect-error-expected "test: hi"
               incorrect-error-exn exn:fail?)

(check-error (error 'test "hi"))
(check-success)

(check-error (/ 1 0) "/: division by zero")
(check-success)

(check-error 3 "some message")
(check-failure expected-error?
               expected-error-message "some message"
               expected-error-value 3)

(check-error (car '()) "another message")
(check-failure incorrect-error?
               incorrect-error-expected "another message"
               incorrect-error-exn exn:fail:contract?)

(check-error (empty) "function call: expected a function after the open parenthesis, but received '()")
(check-success)

(define (markup-message-matcher message)
  (lambda (m)
    (define markup-message (with-output-to-string
                             (lambda () (display-markup m))))
    ;; From the command line, tests/test-engine/racket-tests[.]rkt is
    ;; in the error message
    (and (string-contains? markup-message "racket-tests.rkt")
         (string-contains? markup-message message))))
  
;; rational: x is unbound so this check-error shall not pass
;; (but it also should not raise the syntax error immediately at expansion)
(check-error x "x: this variable is not defined")
(check-failure unexpected-error?
               unexpected-error-exn      exn:fail:syntax?
               unexpected-error-expected "x: this variable is not defined"
               unexpected-error/markup-error-markup
               ;; Ideally, we want to see the message "x: this variable is not defined" in
               ;; the exception. Unfortunately, the #%top binding is not from *sl.
               (markup-message-matcher "x: unbound identifier"))
                      
     
(check-error (#%app/isl 123) "function call: expected a function after the open parenthesis, but received 123")
(check-success)

(check-error (#%app/bsl 123) "function call: expected a function after the open parenthesis, but found a number")
(check-failure unexpected-error?
               unexpected-error-exn      exn:fail:syntax?
               unexpected-error-expected "function call: expected a function after the open parenthesis, but found a number"
               unexpected-error/markup-error-markup
               (markup-message-matcher
                "function call: expected a function after the open parenthesis, but found a number"))

(check-error define/isl "define: expected an open parenthesis before define, but found none")
(check-failure unexpected-error?
               unexpected-error-exn      exn:fail:syntax?
               unexpected-error-expected "define: expected an open parenthesis before define, but found none"
               unexpected-error/markup-error-markup
               (markup-message-matcher
                "define: expected an open parenthesis before define, but found none"))

(define (create n)
  (make-ball n n 'blue))

(check-member-of (create 1) (make-ball 1 2 'blue) (make-ball 1 1 'blue) (make-ball 1 2 'red) 'red)
(check-success)

(check-member-of 1 1 1 1 1)
(check-success)

(check-member-of (create 2) (make-ball 1 2 'blue) (make-ball 1 1 'blue) (make-ball 1 2 'red) 'fails)
(check-failure not-mem?
               not-mem-actual (ball 2 2 'blue)
               not-mem-set (list (ball 1 2 'blue) (ball 1 1 'blue) (ball 1 2 'red) 'fails))

(check-range 5 0 10)
(check-success)
(check-range 0 0 10)
(check-success)
(check-range 10 0 10)
(check-success)

(check-range 11 0 10)
(check-failure not-range?
               not-range-actual 11
               not-range-min 0
               not-range-max 10)

(check-range 5.01 0 10.5)
(check-success)

(check-range 0.0 0 10.5)
(check-success)

(check-range 10.5 0 10.5)
(check-success)

(check-range 10.5001 0 10.5)
(check-failure not-range?
               not-range-actual 10.5001
               not-range-min 0
               not-range-max 10.5)

;; ---------------------------------------------------------------------------------------------------
;; MF: from DVH 

(define (random-elt ls) (list-ref ls (random (length ls))))

(check-random (random-elt (build-list 100 values))
              (list-ref (build-list 100 values) (random 100)))
(check-success)

(define (f _x)
  (list (random 10) (random 20)))

(define (g _x)
  (list (random 10) (random 20) (random 30) (random 40)))

(check-random (f 0) (list (random 10) (random 20)))
(check-success)

(check-random (g 0)
              (let* ((x4 (random 40))
                     (x3 (random 30))
                     (x2 (random 20))
                     (x1 (random 10)))
                (list x1 x2 x3 x4)))
(check-failure unequal?)

(define (h _x) (list (random 50) (random 20) (random 100) (random 70)))

(check-random (h 0) (list (random 50) (random 20) (random 100) (random 70)))
(check-success)

(check-random (h 0) (list (random 20) (random 50) (random 70) (random 100)))
(check-failure unequal?)

(check-property
 (for-all ((a Integer)
           (b Integer))
   (= (+ a b) (+ b a))))
(check-success)

(check-property
 (for-all ((a Integer)
           (b Integer))
   (= (+ a b) (+ b a 1))))
(check-failure property-fail?)

(check-property
 (for-all ((a Integer)
           (b Integer))
   (= (/ a b) (/ b a))))
(check-failure property-fail?)

; check for malformed property
(check-property
 (for-all ((minutes)) #t))
(check-failure property-error?)
