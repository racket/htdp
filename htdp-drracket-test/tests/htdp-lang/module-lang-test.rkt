;; From: drracket/drracket-test/tests/drracket/module-lang-test.rkt
;; At: e94a69c80ae82954ec9ce6ebdec784a99f2b9b98
;; Infrastructure: Robby
;; Test author: Shu-Hung
#lang at-exp racket/base

(require "private/module-lang-test-utils.rkt"
         "private/drracket-test-util.rkt"
         framework
         (only-in racket/gui/base sleep/yield)
         drracket/private/stack-checkpoint
         racket/list
         racket/class)
(provide run-test)

;; set up for tests that need external files
(write-test-modules
 (module module-lang-test-tmp1 mzscheme
   (provide (all-from-except mzscheme +)
            x)
   (define x 1))
 (module module-lang-test-tmp2 mzscheme
   (provide e)
   (define e #'1))
 (module module-lang-test-tmp3 mzscheme
   (define-syntax (bug-datum stx)
     (syntax-case stx ()
       [(dat . thing)
        (number? (syntax-e (syntax thing)))
        (syntax/loc stx (#%datum . thing))]))
   (provide #%module-begin [rename bug-datum #%datum]))
 (module module-lang-test-tmp4 racket/base
   (/ 888 2)
   (provide (except-out (all-from-out racket/base) #%top-interaction)))
 (module module-lang-test-syn-error racket/base
   (lambda)))

(test '("#lang racket\n" xml-box)
      #f
      @t{'(a () "x")})

(test @t{#lang htdp/bsl
         1 2 3}
      #f
      "1\n2\n3")

;; Test that big-bang is in the stacktrace if the state-expr errors at start time.
(test @t{
 #lang htdp/isl+

 (require 2htdp/image)
 (require 2htdp/universe)

 (big-bang (+ 2 #false)
   (to-draw (lambda (s) (empty-scene 200 200))))

}
      #f
      #rx"[+]: expects a number(?: as 2nd argument)?, given #false"
      #:extra-assert
      (λ (defs ints #:stacks stacks #:test test)
        (and (for*/or ([stack (in-list stacks)]
                       #:when stack
                       [loc (in-list (viewable-stack->red-arrows-backtrace-srclocs stack))])
               (regexp-match? #rx"unsaved-editor:6:10"
                              (srcloc->string loc)))
             ;; ^ (+ 2 #false) is in the backtrace
             (for*/or ([stack (in-list stacks)]
                       #:when stack
                       [loc (in-list (viewable-stack->red-arrows-backtrace-srclocs stack))])
               (regexp-match? #rx"unsaved-editor:6:0"
                              (srcloc->string loc)))
             ;; ^ big-bang is in the backtrace, not some internal htdp modules
             (equal?
              (remove-duplicates
               (for/list ([range (send defs get-highlighted-ranges)])
                 (cons (text:range-start range) (text:range-end range))))
              (regexp-match-positions #rx"[(][+] 2.*false[)]"
                                      (test-definitions test)))
             ;; ^ (+ 2 #false) is highlighted
             )))

;; Test that big-bang is highlighted for (initial) check-with errors
;; Note that, however, this only works when big-bang check-with fails at start time.
(test @t{
 #lang htdp/isl+

 (require 2htdp/image)
 (require 2htdp/universe)

 (big-bang -1
   (to-draw (lambda (s) (empty-scene 200 200)))
   (check-with string?))

}
      #f
      #rx"check-with: the initial expression.*fails to pass.*string[?] test"
      #:extra-assert
      (λ (defs ints #:stacks stacks #:test test)
        (and (for*/or ([stack (in-list stacks)]
                       #:when stack
                       [loc (in-list (viewable-stack->red-arrows-backtrace-srclocs stack))])
               (regexp-match? #rx"unsaved-editor:6:0"
                              (srcloc->string loc)))
             ;; ^ big-bang is in the backtrace, not some internal htdp modules
             (equal?
              (remove-duplicates
               (for/list ([range (send defs get-highlighted-ranges)])
                 (cons (text:range-start range) (text:range-end range))))
              (regexp-match-positions #rx"[(]big-bang.*[(]check-with string[?][)][)]"
                                      (test-definitions test)))
             ;; ^ big-bang is highlighted
             )))

;; Needs 2htdp/universe fixing: this does not work at the moment (v8.15)
;; check-with errors triggered by on-{tick,key} etc do not work.
#;
(test @t{
 #lang htdp/isl+

 (require 2htdp/image)
 (require 2htdp/universe)

 (big-bang -1
   (to-draw (lambda (n) (empty-scene 200 200)))
   (on-tick (lambda (n) "oops"))
   (check-with number?))

}
      #f
      #rx"check-with: on-tick handler.*fails to pass.*number[?] test"
      #:extra-assert
      (λ (defs ints #:stacks stacks #:test test)
        (and (for*/or ([stack (in-list stacks)]
                       #:when stack
                       [loc (in-list (viewable-stack->red-arrows-backtrace-srclocs stack))])
               (regexp-match? #rx"unsaved-editor:6:0"
                              (srcloc->string loc)))
             ;; ^ user code is in the backtrace, not some internal htdp modules
             (equal?
              (remove-duplicates
               (for/list ([range (send defs get-highlighted-ranges)])
                 (cons (text:range-start range) (text:range-end range))))
              (regexp-match-positions #rx"[(]big-bang.*[(]check-with number[?][)][)]"
                                      (test-definitions test)))
             ;; ^ user code is highlighted (if not on-tick, at least it should be big-bang)
             )))

(test @t{
 #lang htdp/isl+

 (check-expect (+ 123 45 6) even?)

}
      #f
      #rx"check-expect.*function"
      #:extra-assert
      (λ (defs ints #:stacks stacks #:test test)
        (and (for*/or ([stack (in-list stacks)]
                       #:when stack
                       [loc (in-list (viewable-stack->red-arrows-backtrace-srclocs stack))])
               (regexp-match? #rx"unsaved-editor:3:0"
                              (srcloc->string loc)))
             ;; ^ check-expect is in the backtrace, not some internal test-engine modules
             (equal?
              (remove-duplicates
               (for/list ([range (send defs get-highlighted-ranges)])
                 (cons (text:range-start range) (text:range-end range))))
              (regexp-match-positions #rx"[(]check-expect.*[?][)]"
                                      (test-definitions test)))
             ;; ^ check-expect is highlighted
             )))

(test @t{
 #lang htdp/isl+

 (check-expect (sqrt 2) (sqrt 2))

}
      #f
      #rx"check-expect.*inexact"
      #:extra-assert
      (λ (defs ints #:stacks stacks #:test test)
        (and (for*/or ([stack (in-list stacks)]
                       #:when stack
                       [loc (in-list (viewable-stack->red-arrows-backtrace-srclocs stack))])
               (regexp-match? #rx"unsaved-editor:3:0"
                              (srcloc->string loc)))
             ;; ^ check-expect is in the backtrace, not some internal test-engine modules
             (equal?
              (remove-duplicates
               (for/list ([range (send defs get-highlighted-ranges)])
                 (cons (text:range-start range) (text:range-end range))))
              (regexp-match-positions #rx"[(]check-expect.*sqrt 2[)][)]"
                                      (test-definitions test)))
             ;; ^ check-expect is highlighted
             )))

(test @t{
 #lang htdp/isl+
 (define p (make-posn 7 3))
 (check-expect posn-x 7)

}
      #f
      #rx"Ran 1 test.\n0 tests passed."
      #|
      check-expect encountered the following error instead of the expected value, 7. 
         ::  at line 3, column 0 first argument of equality cannot be a function, given posn-x
      at line 3, column 0
      |#
      #:extra-assert
      (λ (defs ints #:test test)
        (define re
          (pregexp
           (string-append
            "check-expect[ a-z]+error.*[^\n]+\n"
            ".*::.*at line 3, column 0 first argument.*function.*given posn-x[^\n]*\n"
            "at line 3, column 0")))
        ;; Includes the flattened test result snips.
        (define full-ints-text
          (send ints get-text (send ints paragraph-start-position 2) 'eof #t))
        (define passed?
          (regexp-match? re full-ints-text))
        (unless passed?
          (eprintf "FAILED line ~a: ~a\n  extra assertion expected: ~s\n\n  got: ~a\n"
                   (test-line test)
                   (test-definitions test)
                   re
                   full-ints-text))
        passed?))

(test @t{
 #lang htdp/isl+


 (check-random (+ (random 5) (sqrt 2))
               (+ (random 5) (sqrt 2)))

}
      #f
      #rx"check-random.*inexact"
      #:extra-assert
      (λ (defs ints #:stacks stacks #:test test)
        (and (for*/or ([stack (in-list stacks)]
                       #:when stack
                       [loc (in-list (viewable-stack->red-arrows-backtrace-srclocs stack))])
               (regexp-match? #rx"unsaved-editor:4:0"
                              (srcloc->string loc)))
             ;; ^ check-random is in the backtrace, not some internal test-engine modules
             (equal?
              (remove-duplicates
               (for/list ([range (send defs get-highlighted-ranges)])
                 (cons (text:range-start range) (text:range-end range))))
              (regexp-match-positions #rx"[(]check-random.*sqrt 2[)][)][)]"
                                      (test-definitions test)))
             ;; ^ check-random is highlighted
             )))

(test @t{
 #lang htdp/isl+

  (check-within (sqrt 2) 3/2 "0.1")

}
      #f
      #rx"check-within.*\"0[.]1\".*not inexact"
      #:extra-assert
      (λ (defs ints #:stacks stacks #:test test)
        (and (for*/or ([stack (in-list stacks)]
                       #:when stack
                       [loc (in-list (viewable-stack->red-arrows-backtrace-srclocs stack))])
               (regexp-match? #rx"unsaved-editor:3:1"
                              (srcloc->string loc)))
             ;; ^ check-within is in the backtrace, not some internal test-engine modules
             (equal?
              (remove-duplicates
               (for/list ([range (send defs get-highlighted-ranges)])
                 (cons (text:range-start range) (text:range-end range))))
              (regexp-match-positions #rx"[(]check-within.*0[.]1\"[)]"
                                      (test-definitions test)))
             ;; ^ check-within is highlighted
             )))

;; NOTE. If check-satisfied no longer errs immediately in the future,
;; merge this test into the last check-satisfied test below.
(test @t{
 #lang htdp/isl+

  (check-satisfied (+ 2 2) 4)

}
      #f
      #rx"check-satisfied: expect.*function.*one argument.*second position.*iven 4"
      #:extra-assert
      (λ (defs ints #:stacks stacks #:test test)
        (and (for*/or ([stack (in-list stacks)]
                       #:when stack
                       [loc (in-list (viewable-stack->red-arrows-backtrace-srclocs stack))])
               (regexp-match? #rx"unsaved-editor:3:1"
                              (srcloc->string loc)))
             ;; ^ check-satisfied is in the backtrace, not some internal test-engine modules
             (equal?
              (remove-duplicates
               (for/list ([range (send defs get-highlighted-ranges)])
                 (cons (text:range-start range) (text:range-end range))))
              (regexp-match-positions #rx"[(]check-satisfied.*4[)]"
                                      (test-definitions test)))
             ;; ^ check-satisfied is highlighted
             )))

;; NOTE. If the error-check printing bug is fixed, replace the printed procedure
;; #<procedure> with (appropriately escaped) (lambda (a1 a2) ...)
(test @t{
 #lang htdp/isl+

  (check-satisfied 4 (lambda (n bad) (even? n)))

}
      #f
      #rx"check-satisfied: expect.*function.*one argument.*second position.*#<procedure>"
      #:extra-assert
      (λ (defs ints #:stacks stacks #:test test)
        (and (for*/or ([stack (in-list stacks)]
                       #:when stack
                       [loc (in-list (viewable-stack->red-arrows-backtrace-srclocs stack))])
               (regexp-match? #rx"unsaved-editor:3:1"
                              (srcloc->string loc)))
             ;; ^ check-satisfied is in the backtrace, not some internal test-engine modules
             (equal?
              (remove-duplicates
               (for/list ([range (send defs get-highlighted-ranges)])
                 (cons (text:range-start range) (text:range-end range))))
              (regexp-match-positions #rx"[(]check-satisfied.*[(]even[?] n[)][)][)]"
                                      (test-definitions test)))
             ;; ^ check-satisfied is highlighted
             )))

;; NOTE.
;; 1. After the error-check printing bug is fixed, add the "function:" prefix
;;    to all the functions in the error messsage in this test.
;; 2. The column numbers after "::" in each error could be further improved/refined,
;;    but the srclocs should include at least the line numbers of the user code.
;; 3. Also remember to test this in a _saved_ buffer.
(test @t{#lang htdp/isl

         (define local-even
           (local [(define (my-even m k)
                     (even? (+ m k)))]
             my-even))

         (define (real-my-even m k)
           (even? (+ m k)))

         (check-satisfied (rest (list 3514)) empty)
         (check-satisfied 4 local-even)
         (check-satisfied 4 real-my-even)}
      #f
      #px"^Ran 3 tests[.]\\s+0 tests passed[.]"
      #t
      #:extra-assert
      (λ (defs ints #:test test)
        (define re
          (pregexp
           @t{^Ran 3 tests[.]
              0 tests passed[.]

              Check failures:\s*
                +check-satisfied for empty encountered an error[.]\s*
                +:: +at line 11, column 0 +function call: expect.+function.+open parenthesis.+received '[(][)]\s*
              at line 11, column 0 *caused by expression *at line 11, column 0
                +check-satisfied for local-even encountered an error[.]\s*
                +:: +at line 12, column 0 +my-even: expect.+2 arguments.+found only 1\s*
              at line 12, column 0 *caused by expression *at line 12, column 0
                +check-satisfied for real-my-even encountered an error[.]\s*
                +:: +at line 13, column 0 +check-satisfied: expect.+function.+one argument.+second position.+real-my-even\s*
              at line 13, column 0 *caused by expression *at line 13, column 0
              > }))
        ;; Includes the flattened test result snips.
        (define full-ints-text
          (send ints get-text (send ints paragraph-start-position 2) 'eof #t))
        (define passed?
          (regexp-match? re full-ints-text))
        (unless passed?
          (eprintf "FAILED line ~a: ~a\n  extra assertion expected: ~s\n\n  got: ~a\n"
                   (test-line test)
                   (test-definitions test)
                   re
                   full-ints-text)
          (flush-output (current-error-port))
          (sleep/yield 0.1))
        passed?))

(define (close-current-tab-and-open-new-tab filename)
  (define path (in-here/path filename))
  (define drs (wait-for-drracket-frame))
  (test:menu-select "File" "New Tab")
  (case (system-type 'os)
    [(macosx windows)
     (test:menu-select "Windows" (format "Tab 1: ~a" filename))
     (test:menu-select "File" "Close Tab")]
    [(unix)
     (test:menu-select "Tabs" (format "Tab 1: ~a" filename))
     (test:menu-select "File" "Close")])
  (when (file-exists? path)
    (delete-file path)))

(let ([filename @t{gh208-pr229-islplus.rkt}])
(test #:before-execute (λ () (save-drracket-window-as
                              (string->path (in-here/path filename))))
      #:after-test (λ () (close-current-tab-and-open-new-tab filename))
      #:wait-for-drracket-frame-after-test? #t
      @t{
 #lang htdp/isl+

 (define (my-add1 n) (+ n 1))
 my-add1
 (check-expect my-add1 2)

 (let ([keep-parity (lambda (m)
                      (+ m 2))])
   keep-parity)

 (local [(define alt-parity (lambda (m)
                              (- 1 m)))]
   alt-parity)

 (let ()
   (lambda (m)
     (+ m 2)))

 (local [(define lam-in-if
          (if (> (random 10) 5)
              (lambda (x) (+ x 5))
              (lambda (y) (* y 2))))]
  lam-in-if)

}
      #f
      @rx{^my-add1
          keep-parity
          alt-parity
          [(]lambda [(]a1[)] [.][.][.][)]
          lam-in-if
          Ran 1 test[.]
          0 tests passed[.]}
      #:extra-assert
      (λ (defs ints #:test test)
        (define ^\n "[^\n]+")
        (define re
          (pregexp
           @t{::\s+in @(regexp-quote filename), line 5, column 0@|^\n|function@|^\n|given my-add1}))
        ;; Includes the flattened test result snips.
        (define full-ints-text
          (send ints get-text (send ints paragraph-start-position 2) 'eof #t))
        (define passed?
          (regexp-match? re full-ints-text))
        (unless passed?
          (eprintf "FAILED line ~a: ~a\n  extra assertion expected: ~s\n\n  got: ~a\n"
                   (test-line test)
                   (test-definitions test)
                   re
                   full-ints-text)
          (flush-output (current-error-port))
          (sleep/yield 0.1))
        passed?)))

;; Run the same test, but in an unsaved buffer.
(test @t{
 #lang htdp/isl+

 (define (my-add1 n) (+ n 1))
 my-add1
 (check-expect my-add1 2)

 (let ([keep-parity (lambda (m)
                      (+ m 2))])
   keep-parity)

 (local [(define alt-parity (lambda (m)
                              (- 1 m)))]
   alt-parity)

 (let ()
   (lambda (m)
     (+ m 2)))

 (local [(define lam-in-if
          (if (> (random 10) 5)
              (lambda (x) (+ x 5))
              (lambda (y) (* y 2))))]
  lam-in-if)

}
      #f
      @rx{^my-add1
          keep-parity
          alt-parity
          [(]lambda [(]a1[)] [.][.][.][)]
          lam-in-if
          Ran 1 test[.]
          0 tests passed[.]}
      #:extra-assert
      (λ (defs ints)
        (regexp-match? #px"::\\s+at line 5, column 0[^\n]+function[^\n]+given my-add1"
                       ;; Includes the flattened test result snips.
                       (send ints get-text (send ints paragraph-start-position 2) 'eof #t))))

(let ([filename @t{gh208-pr229-isl.rkt}])
(test #:before-execute (λ () (save-drracket-window-as
                              (string->path (in-here/path filename))))
      #:after-test (λ () (close-current-tab-and-open-new-tab filename))
      #:wait-for-drracket-frame-after-test? #t
      @t{
 #lang htdp/isl

 (define (my-add1 n) (+ n 1))
 my-add1
 (check-expect my-add1 2)

 (let ([keep-parity (lambda (m)
                      (+ m 2))])
   keep-parity)

 (local [(define alt-parity (lambda (m)
                              (- 1 m)))]
   alt-parity)

}
      #f
      @rx{^function:my-add1
          function:keep-parity
          function:alt-parity
          Ran 1 test[.]
          0 tests passed[.]}
      #:extra-assert
      (λ (defs ints)
        (define ^\n "[^\n]+")
        (regexp-match?
         (pregexp
          @t{::\s+in @(regexp-quote filename), line 5, column 0@|^\n|function@|^\n|given function:my-add1})
         ;; Includes the flattened test result snips.
         (send ints get-text (send ints paragraph-start-position 2) 'eof #t)))))

;; Run the same test, but in an unsaved buffer.
(test @t{
 #lang htdp/isl

 (define (my-add1 n) (+ n 1))
 my-add1
 (check-expect my-add1 2)

 (let ([keep-parity (lambda (m)
                      (+ m 2))])
   keep-parity)

 (local [(define alt-parity (lambda (m)
                              (- 1 m)))]
   alt-parity)

}
      #f
      @rx{^function:my-add1
          function:keep-parity
          function:alt-parity
          Ran 1 test[.]
          0 tests passed[.]}
      #:extra-assert
      (λ (defs ints)
        (regexp-match? #px"::\\s+at line 5, column 0[^\n]+function[^\n]+given function:my-add1"
                       ;; Includes the flattened test result snips.
                       (send ints get-text (send ints paragraph-start-position 2) 'eof #t))))

(let ([filename @t{htdp-tests-intm-lam-map.rkt}])
(test #:before-execute (λ () (save-drracket-window-as
                              (string->path (in-here/path filename))))
      #:after-test (λ () (close-current-tab-and-open-new-tab filename))
      #:wait-for-drracket-frame-after-test? #t
      @t{
#lang htdp/isl+
   (map (lambda (x y) (+ x y)) (list 2 3 4))
}
      #f
      @rx{map: first argument must be a function that expects one argument,
          given @regexp-quote{(lambda (a1 a2) ...)}}
      #:extra-assert
      (λ (defs ints #:stacks stacks #:test test)
        (and (for*/or ([stack (in-list stacks)]
                       #:when stack
                       [loc (in-list (viewable-stack->red-arrows-backtrace-srclocs stack))])
               (regexp-match? @rx{@(regexp-quote filename):2:3}
                              (srcloc->string loc)))
             ;; ^ foldr is in the backtrace, not some internal HtDP modules
             (equal?
              (remove-duplicates
               (for/list ([range (send defs get-highlighted-ranges)])
                 (cons (text:range-start range) (text:range-end range))))
              (regexp-match-positions #rx"[(]map.*3 4[)][)]"
                                      (test-definitions test)))
             ;; ^ foldr is highlighted
             ))))

(let ([filename @t{htdp-tests-intm-lam-foldr2.rkt}])
(test #:before-execute (λ () (save-drracket-window-as
                              (string->path (in-here/path filename))))
      #:after-test (λ () (close-current-tab-and-open-new-tab filename))
      #:wait-for-drracket-frame-after-test? #t
      @t{
#lang htdp/isl+
   (foldr (lambda (x y) (+ x y)) 0 (list 2 3 4) (list 2 3 4))
}
      #f
      @rx{foldr: first argument must be a function that expects three arguments,
          given @regexp-quote{(lambda (a1 a2) ...)}}
      #:extra-assert
      (λ (defs ints #:stacks stacks #:test test)
        (and (for*/or ([stack (in-list stacks)]
                       #:when stack
                       [loc (in-list (viewable-stack->red-arrows-backtrace-srclocs stack))])
               (regexp-match? @rx{@(regexp-quote filename):2:3}
                              (srcloc->string loc)))
             ;; ^ foldr is in the backtrace, not some internal HtDP modules
             (equal?
              (remove-duplicates
               (for/list ([range (send defs get-highlighted-ranges)])
                 (cons (text:range-start range) (text:range-end range))))
              (regexp-match-positions #rx"[(]foldr.*3 4[)][)]"
                                      (test-definitions test)))
             ;; ^ foldr is highlighted
             ))))

(let ([filename @t{htdp-tests-intm-lam-foldr3.rkt}])
(test #:before-execute (λ () (save-drracket-window-as
                              (string->path (in-here/path filename))))
      #:after-test (λ () (close-current-tab-and-open-new-tab filename))
      #:wait-for-drracket-frame-after-test? #t
      @t{
#lang htdp/isl+
   (foldr (lambda (x y z) (+ x y z)) 0 (list 2 3 4))
}
      #f
      @rx{foldr: first argument must be a function that expects two arguments,
          given @regexp-quote{(lambda (a1 a2 a3) ...)}}
      #:extra-assert
      (λ (defs ints #:stacks stacks #:test test)
        (and (for*/or ([stack (in-list stacks)]
                       #:when stack
                       [loc (in-list (viewable-stack->red-arrows-backtrace-srclocs stack))])
               (regexp-match? @rx{@(regexp-quote filename):2:3}
                              (srcloc->string loc)))
             ;; ^ foldr is in the backtrace, not some internal HtDP modules
             (equal?
              (remove-duplicates
               (for/list ([range (send defs get-highlighted-ranges)])
                 (cons (text:range-start range) (text:range-end range))))
              (regexp-match-positions #rx"[(]foldr.*3 4[)][)]"
                                      (test-definitions test)))
             ;; ^ foldr is highlighted
             ))))

(test @t{#lang htdp/isl
         (check-expect (* 2 3) 6)
         (check-expect (+ 2 3) 5)}
      #f
      #rx"^Both tests passed!$")

(test @t{#lang htdp/isl}
      ;; REPL
      @t{(check-expect (* 2 3) 6)
         (check-expect (+ 2 3) 5)}
      #rx"^The test passed!\nThe test passed!$")

(test @t{#lang htdp/isl
         (check-expect (* 2 3) 6)
         (check-expect (* 2 3) 5)}
      #f
      #rx"^Ran 2 tests[.]\n1 of the 2 tests failed[.].*Check failures:")

(test @t{#lang htdp/isl}
      ;; REPL
      @t{(check-expect (* 2 3) 6)
         (check-expect (* 2 3) 5)}
      #rx"^The test passed!\nRan 1 test[.]\n0 tests passed[.].*Check failures:")

(test @t{#lang htdp/isl}
      ;; REPL
      @t{(check-expect (* 2 3) 5)
         (check-expect (* 2 3) 6)}
      #rx"^Ran 1 test[.]\n0 tests passed[.].*Check failures:.*\nThe test passed!$")

(test @t{#lang htdp/isl
         (check-expect (* 2 3) 6)
         (check-expect (* 2 3) 5)
         (check-expect (+ 2 3) 5)}
      ;; REPL
      @t{(check-expect (+ 4 5) 9)
         (check-expect (+ 6 7) 42)
         (check-expect (* 8 9) 72)
         (check-expect (error 'oops) 111)}
      #px"^Ran 3 tests[.]\\s+1 of the 3 tests failed[.]"
      #t
      #:extra-assert
      (λ (defs ints #:test test)
        (define re
          (pregexp
           @t{^Ran 3 tests[.]
              1 of the 3 tests failed[.]

              Check failures:\s*
                +Actual value 6 differs from 5, the expected value[.]\s*
              at line 3, column 0
              > @(regexp-quote (test-interactions test))
              The test passed!
              Ran 1 test[.]
              0 tests passed[.]

              Check failures:\s*
                +Actual value 13 differs from 42, the expected value[.]\s*
              at line 10, column 0
              The test passed!
              Ran 1 test[.]
              0 tests passed[.]

              Check failures:\s*
                +check-expect encountered the following error instead of the expected value, 111[.]\s*
                +:: +at line 12, column 14 oops:\s*
              at line 12, column 0 *caused by expression *at line 12, column 14
              > }))
        ;; Includes the flattened test result snips.
        (define full-ints-text
          (send ints get-text (send ints paragraph-start-position 2) 'eof #t))
        (define passed?
          (regexp-match? re full-ints-text))
        (unless passed?
          (eprintf "FAILED line ~a: ~a\n  extra assertion expected: ~s\n\n  got: ~a\n"
                   (test-line test)
                   (test-definitions test)
                   re
                   full-ints-text)
          (flush-output (current-error-port))
          (sleep/yield 0.1))
        passed?))

(fire-up-drracket-and-run-tests run-test)

;; Test mode:
(module test racket/base
  (require racket/port syntax/location)
  (define-values (inp outp) (make-pipe))
  (define tee-error-port (open-output-bytes 'tee-stderr))
  (define stderr (current-error-port))
  (void
   (thread
    (λ () (copy-port inp tee-error-port stderr))))
  (exit-handler
   (let ([old-exit-hdlr (exit-handler)])
     (λ (code)
       (define stderr-content-length
         (bytes-length (get-output-bytes tee-error-port #t)))
       (cond
         [(and (number? code) (zero? code) (> stderr-content-length 0))
          (write-string "non-empty stderr\n" stderr)
          (old-exit-hdlr 1)]
         [else
          (old-exit-hdlr code)]))))
  (void (putenv "PLTDRTEST" "yes"))
  (eval-jit-enabled #f)
  (parameterize ([current-error-port outp])
    (dynamic-require (quote-module-path "..") #f))
  (module config info
    (define timeout 800)))
